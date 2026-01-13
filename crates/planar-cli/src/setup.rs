use anyhow::{anyhow, Context};
use console::{Emoji, style};
use futures_util::StreamExt;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use planarc::common;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use tokio::fs::{self, File};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

#[derive(Deserialize)]
struct Manifest {
    files: HashMap<String, String>,
}

static TICK: Emoji<'_, '_> = Emoji("✔ ", "");

pub async fn run(force: bool, url: String) -> anyhow::Result<()> {
    let client = reqwest::Client::new();
    
    fs::create_dir_all(&common::local_dir()).await?;

    println!("{} {}Checking registry...", style("planar").bold().cyan(), Emoji("📡", ""));
    
    let manifest: Manifest = client
        .get(format!("{}/manifest.json", url))
        .send()
        .await?
        .json()
        .await
        .context("Failed to fetch manifest")?;

    let multi = MultiProgress::new();
    let mut download_tasks = vec![];
    let mut cached_count = 0;
    let mut synced_count = 0;

    
    let pb_style = ProgressStyle::with_template(
        "{spinner:.green} {msg:>20} {bar:20.cyan/blue} {bytes:>10}/{total_bytes:>10}"
    )?.progress_chars("█▉▊▋▌▍▎▏  ");

    let target_files: Vec<(String, String)> = manifest.files
        .into_iter()
        .filter(|(filename, _)| common::is_match(filename)) 
        .collect();

    for (filename, expected_hash) in target_files {
        
        let file_path = common::local_dir().join(&filename);
    

        let is_cached = if !force && file_path.exists() {
            if let Ok(actual_hash) = compute_file_hash(&file_path).await {
                actual_hash == expected_hash
            } else { false }
        } else { false };

        if is_cached {
            
            println!("  {} {} {}", style(TICK).green(), style(filename).dim(), style("(cached)").dim());
            cached_count += 1;
            continue;
        }

        let client = client.clone();
        let pb = multi.add(ProgressBar::new(0));
        pb.set_style(pb_style.clone());
        pb.set_message(filename.clone());

        let filename_clone = filename.clone();
        let expected_hash_clone = expected_hash.clone();
        let m_clone = multi.clone();
        let url = url.clone();
        download_tasks.push(tokio::spawn(async move {
            let url = format!("{}/{}", url, filename_clone);
            let res = client.get(url).send().await?;
            let total_size = res.content_length().unwrap_or(0);
            pb.set_length(total_size);

            let mut stream = res.bytes_stream();
            let tmp_path = file_path.with_extension("tmp");
            let mut file = File::create(&tmp_path).await?;
            let mut hasher = Sha256::new();

            while let Some(item) = stream.next().await {
                let chunk = item?;
                file.write_all(&chunk).await?;
                hasher.update(&chunk);
                pb.set_position(pb.position() + chunk.len() as u64);
            }

            file.flush().await?;
            let actual_hash = hex::encode(hasher.finalize());

            if actual_hash != expected_hash_clone {
                let _ = fs::remove_file(tmp_path).await;
                return Err(anyhow!("Hash mismatch for {}", filename_clone));
            }

            fs::rename(tmp_path, file_path).await?;
            
            pb.finish_and_clear();
            m_clone.println(format!("  {} {} {}", style(TICK).green(), filename_clone, style("(synced)").green()))?;
            
            anyhow::Ok(())
        }));

    }

    if !download_tasks.is_empty() {
        let results = futures_util::future::join_all(download_tasks).await;
        for res in results {
            match res? {
                Ok(_) => synced_count += 1,
                Err(e) => eprintln!("  {} {}", style("error:").red(), e),
            }
        }
    }

    println!(
        "\n{} Done! {} cached, {} synced.",
        style(TICK).green(),
        style(cached_count).cyan(),
        style(synced_count).green()
    );

    Ok(())
}

async fn compute_file_hash(path: &Path) -> anyhow::Result<String> {
    let mut file = File::open(path).await?;
    let mut hasher = Sha256::new();
    let mut buffer = vec![0; 8192];

    loop {
        let n = file.read(&mut buffer).await?;
        if n == 0 { break; }
        hasher.update(&buffer[..n]);
    }
    Ok(hex::encode(hasher.finalize()))
}