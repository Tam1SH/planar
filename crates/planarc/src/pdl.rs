/**Typed node `attribute`

This node has these fields:

- `name`: `identifier` ([`Identifier`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Attribute<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Attribute<'tree> {
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Attribute<'tree> {
    type WithLifetime<'a> = Attribute<'a>;
    const KIND: &'static str = "attribute";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "attribute" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "attribute");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `block`

This node has named children of type `{match_stmt | query_definition}*`:

- [`MatchStmt`]
- [`QueryDefinition`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Block<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Block<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Block<'tree> {
    type Child = anon_unions::MatchStmt_QueryDefinition<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Block<'tree> {
    type WithLifetime<'a> = Block<'a>;
    const KIND: &'static str = "block";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "block" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "block");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `call_func`

This node has these fields:

- `arg`: `variable*` ([`Variable`])
- `function`: `fqmn` ([`Fqmn`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct CallFunc<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> CallFunc<'tree> {
    /**Get the children of field `arg`.

These children have type `variable*` ([`Variable`])*/
    #[inline]
    pub fn args<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Variable<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("arg", &mut c.0)
            .map(<Variable<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the field `function`.

This child has type `fqmn` ([`Fqmn`])*/
    #[inline]
    pub fn function(&self) -> ::type_sitter::NodeResult<'tree, Fqmn<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("function")
            .map(<Fqmn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for CallFunc<'tree> {
    type WithLifetime<'a> = CallFunc<'a>;
    const KIND: &'static str = "call_func";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "call_func" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "call_func");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `capture`

This node has named children of type `{capture_block | variable}+`:

- [`CaptureBlock`]
- [`Variable`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Capture<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Capture<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Capture<'tree> {
    type Child = anon_unions::CaptureBlock_Variable<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Capture<'tree> {
    type WithLifetime<'a> = Capture<'a>;
    const KIND: &'static str = "capture";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "capture" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "capture");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `capture_block`

This node has named children of type `graph_bind*` ([`GraphBind`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct CaptureBlock<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> CaptureBlock<'tree> {
    /**Get the node's not-extra named children.

These children have type `graph_bind*` ([`GraphBind`])*/
    #[inline]
    pub fn graph_binds<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, GraphBind<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<GraphBind<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for CaptureBlock<'tree> {
    type Child = GraphBind<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for CaptureBlock<'tree> {
    type WithLifetime<'a> = CaptureBlock<'a>;
    const KIND: &'static str = "capture_block";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "capture_block" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "capture_block");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `comment`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Comment<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Comment<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Comment<'tree> {
    type WithLifetime<'a> = Comment<'a>;
    const KIND: &'static str = "comment";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "comment" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "comment");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `edge_definition`

This node has these fields:

- `from`: `type_identifier` ([`TypeIdentifier`])
- `name`: `identifier` ([`Identifier`])
- `to`: `type_identifier` ([`TypeIdentifier`])

And additional named children of type `{pub | relation}+`:

- [`Pub`]
- [`Relation`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct EdgeDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> EdgeDefinition<'tree> {
    /**Get the field `from`.

This child has type `type_identifier` ([`TypeIdentifier`])*/
    #[inline]
    pub fn from(&self) -> ::type_sitter::NodeResult<'tree, TypeIdentifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("from")
            .map(<TypeIdentifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `to`.

This child has type `type_identifier` ([`TypeIdentifier`])*/
    #[inline]
    pub fn to(&self) -> ::type_sitter::NodeResult<'tree, TypeIdentifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("to")
            .map(<TypeIdentifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's non-field not-extra named children.

These children have type `{pub | relation}+`:

- [`Pub`]
- [`Relation`]
*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn others<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, anon_unions::Pub_Relation<'tree>>,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(
                <anon_unions::Pub_Relation<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for EdgeDefinition<'tree> {
    type WithLifetime<'a> = EdgeDefinition<'a>;
    const KIND: &'static str = "edge_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "edge_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "edge_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `extern_block`

This node has named children of type `extern_def_fn*` ([`ExternDefFn`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ExternBlock<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ExternBlock<'tree> {
    /**Get the node's not-extra named children.

These children have type `extern_def_fn*` ([`ExternDefFn`])*/
    #[inline]
    pub fn extern_def_fns<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, ExternDefFn<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<ExternDefFn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for ExternBlock<'tree> {
    type Child = ExternDefFn<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ExternBlock<'tree> {
    type WithLifetime<'a> = ExternBlock<'a>;
    const KIND: &'static str = "extern_block";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "extern_block" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "extern_block");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `extern_def_arg`

This node has these fields:

- `arg`: `identifier` ([`Identifier`])
- `type`: `type_annotation` ([`TypeAnnotation`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ExternDefArg<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ExternDefArg<'tree> {
    /**Get the field `arg`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn arg(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("arg")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `type`.

This child has type `type_annotation` ([`TypeAnnotation`])*/
    #[inline]
    pub fn r#type(&self) -> ::type_sitter::NodeResult<'tree, TypeAnnotation<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("type")
            .map(<TypeAnnotation<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ExternDefArg<'tree> {
    type WithLifetime<'a> = ExternDefArg<'a>;
    const KIND: &'static str = "extern_def_arg";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "extern_def_arg" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "extern_def_arg");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `extern_def_fn`

This node has named children of type `{extern_def_arg | extern_return | identifier | operator_identifier}+`:

- [`ExternDefArg`]
- [`ExternReturn`]
- [`Identifier`]
- [`OperatorIdentifier`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ExternDefFn<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ExternDefFn<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for ExternDefFn<'tree> {
    type Child = anon_unions::ExternDefArg_ExternReturn_Identifier_OperatorIdentifier<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ExternDefFn<'tree> {
    type WithLifetime<'a> = ExternDefFn<'a>;
    const KIND: &'static str = "extern_def_fn";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "extern_def_fn" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "extern_def_fn");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `extern_definition`

This node has these fields:

- `attributes`: `attribute*` ([`Attribute`])
- `block`: `extern_block` ([`ExternBlock`])

And an optional additional named child of type `pub?` ([`Pub`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ExternDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ExternDefinition<'tree> {
    /**Get the children of field `attributes`.

These children have type `attribute*` ([`Attribute`])*/
    #[inline]
    pub fn attributess<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Attribute<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("attributes", &mut c.0)
            .map(<Attribute<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the field `block`.

This child has type `extern_block` ([`ExternBlock`])*/
    #[inline]
    pub fn block(&self) -> ::type_sitter::NodeResult<'tree, ExternBlock<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("block")
            .map(<ExternBlock<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's only non-field not-extra named child, if it has one.

This child has type `pub?` ([`Pub`])*/
    #[inline]
    pub fn r#pub(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Pub<'tree>>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .filter(|i| {
                ::type_sitter::Node::raw(self)
                    .field_name_for_named_child(*i as _)
                    .is_none()
            })
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<Pub<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ExternDefinition<'tree> {
    type WithLifetime<'a> = ExternDefinition<'a>;
    const KIND: &'static str = "extern_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "extern_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "extern_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `extern_return`

This node has a named child of type `type_annotation` ([`TypeAnnotation`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ExternReturn<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ExternReturn<'tree> {
    /**Get the node's only not-extra named child.

This child has type `type_annotation` ([`TypeAnnotation`])*/
    #[inline]
    pub fn type_annotation(
        &self,
    ) -> ::type_sitter::NodeResult<'tree, TypeAnnotation<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<TypeAnnotation<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for ExternReturn<'tree> {
    type Child = TypeAnnotation<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ExternReturn<'tree> {
    type WithLifetime<'a> = ExternReturn<'a>;
    const KIND: &'static str = "extern_return";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "extern_return" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "extern_return");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `fact_definition`

This node has these fields:

- `name`: `identifier` ([`Identifier`])

And additional named children of type `{attribute | fact_field_definition | pub}*`:

- [`Attribute`]
- [`FactFieldDefinition`]
- [`Pub`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct FactDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> FactDefinition<'tree> {
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's non-field not-extra named children.

These children have type `{attribute | fact_field_definition | pub}*`:

- [`Attribute`]
- [`FactFieldDefinition`]
- [`Pub`]
*/
    #[inline]
    pub fn others<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<
            'tree,
            anon_unions::Attribute_FactFieldDefinition_Pub<'tree>,
        >,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(
                <anon_unions::Attribute_FactFieldDefinition_Pub<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for FactDefinition<'tree> {
    type WithLifetime<'a> = FactDefinition<'a>;
    const KIND: &'static str = "fact_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "fact_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "fact_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `fact_field_definition`

This node has these fields:

- `name`: `identifier` ([`Identifier`])
- `type`: `type_annotation` ([`TypeAnnotation`])

And additional named children of type `attribute*` ([`Attribute`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct FactFieldDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> FactFieldDefinition<'tree> {
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `type`.

This child has type `type_annotation` ([`TypeAnnotation`])*/
    #[inline]
    pub fn r#type(&self) -> ::type_sitter::NodeResult<'tree, TypeAnnotation<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("type")
            .map(<TypeAnnotation<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's non-field not-extra named children.

These children have type `attribute*` ([`Attribute`])*/
    #[inline]
    pub fn attributes<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Attribute<'tree>>,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(<Attribute<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for FactFieldDefinition<'tree> {
    type WithLifetime<'a> = FactFieldDefinition<'a>;
    const KIND: &'static str = "fact_field_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "fact_field_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "fact_field_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `fqmn`

This node has named children of type `identifier+` ([`Identifier`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Fqmn<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Fqmn<'tree> {
    /**Get the node's not-extra named children.

These children have type `identifier+` ([`Identifier`])*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn identifiers<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Identifier<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Fqmn<'tree> {
    type Child = Identifier<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Fqmn<'tree> {
    type WithLifetime<'a> = Fqmn<'a>;
    const KIND: &'static str = "fqmn";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "fqmn" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "fqmn");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `graph_bind`

This node has these fields:

- `left`: `graph_left_statements` ([`GraphLeftStatements`])
- `relation`: `relation` ([`Relation`])
- `right`: `graph_right_statements` ([`GraphRightStatements`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct GraphBind<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> GraphBind<'tree> {
    /**Get the field `left`.

This child has type `graph_left_statements` ([`GraphLeftStatements`])*/
    #[inline]
    pub fn left(&self) -> ::type_sitter::NodeResult<'tree, GraphLeftStatements<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("left")
            .map(
                <GraphLeftStatements<'tree> as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `relation`.

This child has type `relation` ([`Relation`])*/
    #[inline]
    pub fn relation(&self) -> ::type_sitter::NodeResult<'tree, Relation<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("relation")
            .map(<Relation<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `right`.

This child has type `graph_right_statements` ([`GraphRightStatements`])*/
    #[inline]
    pub fn right(
        &self,
    ) -> ::type_sitter::NodeResult<'tree, GraphRightStatements<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("right")
            .map(
                <GraphRightStatements<'tree> as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for GraphBind<'tree> {
    type WithLifetime<'a> = GraphBind<'a>;
    const KIND: &'static str = "graph_bind";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "graph_bind" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "graph_bind");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `graph_left_statements`

This node has a named child of type `{identifier | variable}`:

- [`Identifier`]
- [`Variable`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct GraphLeftStatements<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> GraphLeftStatements<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for GraphLeftStatements<'tree> {
    type Child = anon_unions::Identifier_Variable<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for GraphLeftStatements<'tree> {
    type WithLifetime<'a> = GraphLeftStatements<'a>;
    const KIND: &'static str = "graph_left_statements";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "graph_left_statements" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "graph_left_statements");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `graph_right_statements`

This node has a named child of type `call_func` ([`CallFunc`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct GraphRightStatements<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> GraphRightStatements<'tree> {
    /**Get the node's only not-extra named child.

This child has type `call_func` ([`CallFunc`])*/
    #[inline]
    pub fn call_func(&self) -> ::type_sitter::NodeResult<'tree, CallFunc<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<CallFunc<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for GraphRightStatements<'tree> {
    type Child = CallFunc<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for GraphRightStatements<'tree> {
    type WithLifetime<'a> = GraphRightStatements<'a>;
    const KIND: &'static str = "graph_right_statements";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "graph_right_statements" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "graph_right_statements");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `identifier`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Identifier<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Identifier<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Identifier<'tree> {
    type WithLifetime<'a> = Identifier<'a>;
    const KIND: &'static str = "identifier";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "identifier" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "identifier");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `import_definition`

This node has a named child of type `fqmn` ([`Fqmn`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ImportDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ImportDefinition<'tree> {
    /**Get the node's only not-extra named child.

This child has type `fqmn` ([`Fqmn`])*/
    #[inline]
    pub fn fqmn(&self) -> ::type_sitter::NodeResult<'tree, Fqmn<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<Fqmn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for ImportDefinition<'tree> {
    type Child = Fqmn<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ImportDefinition<'tree> {
    type WithLifetime<'a> = ImportDefinition<'a>;
    const KIND: &'static str = "import_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "import_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "import_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `in_expression`

This node has a named child of type `{list_items | range}`:

- [`ListItems`]
- [`Range`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct InExpression<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> InExpression<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for InExpression<'tree> {
    type Child = anon_unions::ListItems_Range<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for InExpression<'tree> {
    type WithLifetime<'a> = InExpression<'a>;
    const KIND: &'static str = "in_expression";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "in_expression" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "in_expression");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `it`

This node has these fields:

- `it`: `it` ([`unnamed::It`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct It<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> It<'tree> {
    /**Get the field `it`.

This child has type `it` ([`unnamed::It`])*/
    #[inline]
    pub fn it(&self) -> ::type_sitter::NodeResult<'tree, unnamed::It<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("it")
            .map(<unnamed::It<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for It<'tree> {
    type WithLifetime<'a> = It<'a>;
    const KIND: &'static str = "it";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "it" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "it");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `list_items`

This node has named children of type `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}+`:

- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ListItems<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ListItems<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for ListItems<'tree> {
    type Child = anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ListItems<'tree> {
    type WithLifetime<'a> = ListItems<'a>;
    const KIND: &'static str = "list_items";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "list_items" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "list_items");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `match_block`

This node has named children of type `capture*` ([`Capture`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct MatchBlock<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> MatchBlock<'tree> {
    /**Get the node's not-extra named children.

These children have type `capture*` ([`Capture`])*/
    #[inline]
    pub fn captures<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, Capture<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<Capture<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for MatchBlock<'tree> {
    type Child = Capture<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for MatchBlock<'tree> {
    type WithLifetime<'a> = MatchBlock<'a>;
    const KIND: &'static str = "match_block";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "match_block" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "match_block");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `match_stmt`

This node has these fields:

- `query`: `{identifier | query_literal}` ([`Identifier`] | [`QueryLiteral`])

And an additional named child of type `match_block` ([`MatchBlock`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct MatchStmt<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> MatchStmt<'tree> {
    /**Get the field `query`.

This child has type `{identifier | query_literal}`:

- [`Identifier`]
- [`QueryLiteral`]
*/
    #[inline]
    pub fn query(
        &self,
    ) -> ::type_sitter::NodeResult<'tree, anon_unions::Identifier_QueryLiteral<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("query")
            .map(
                <anon_unions::Identifier_QueryLiteral<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's only non-field not-extra named child.

This child has type `match_block` ([`MatchBlock`])*/
    #[inline]
    pub fn match_block(&self) -> ::type_sitter::NodeResult<'tree, MatchBlock<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .filter(|i| {
                ::type_sitter::Node::raw(self)
                    .field_name_for_named_child(*i as _)
                    .is_none()
            })
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<MatchBlock<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for MatchStmt<'tree> {
    type WithLifetime<'a> = MatchStmt<'a>;
    const KIND: &'static str = "match_stmt";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "match_stmt" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "match_stmt");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `node_definition`

This node has these fields:

- `kind`: `fqmn` ([`Fqmn`])

And additional named children of type `{block | pub}+`:

- [`Block`]
- [`Pub`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct NodeDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> NodeDefinition<'tree> {
    /**Get the field `kind`.

This child has type `fqmn` ([`Fqmn`])*/
    #[inline]
    pub fn kind(&self) -> ::type_sitter::NodeResult<'tree, Fqmn<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("kind")
            .map(<Fqmn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's non-field not-extra named children.

These children have type `{block | pub}+`:

- [`Block`]
- [`Pub`]
*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn others<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, anon_unions::Block_Pub<'tree>>,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(
                <anon_unions::Block_Pub<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for NodeDefinition<'tree> {
    type WithLifetime<'a> = NodeDefinition<'a>;
    const KIND: &'static str = "node_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "node_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "node_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `number`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Number<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Number<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Number<'tree> {
    type WithLifetime<'a> = Number<'a>;
    const KIND: &'static str = "number";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "number" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "number");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `operator_identifier`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct OperatorIdentifier<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> OperatorIdentifier<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for OperatorIdentifier<'tree> {
    type WithLifetime<'a> = OperatorIdentifier<'a>;
    const KIND: &'static str = "operator_identifier";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "operator_identifier" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "operator_identifier");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `parenthesized_expression`

This node has named children of type `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}+`:

- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct ParenthesizedExpression<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> ParenthesizedExpression<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for ParenthesizedExpression<'tree> {
    type Child = anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for ParenthesizedExpression<'tree> {
    type WithLifetime<'a> = ParenthesizedExpression<'a>;
    const KIND: &'static str = "parenthesized_expression";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "parenthesized_expression" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "parenthesized_expression");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `property_access`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct PropertyAccess<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> PropertyAccess<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for PropertyAccess<'tree> {
    type WithLifetime<'a> = PropertyAccess<'a>;
    const KIND: &'static str = "property_access";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "property_access" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "property_access");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `pub`

This node has these fields:

- `pkg`: `(pkg)?` ([`symbols::LParenpkgRParen`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Pub<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Pub<'tree> {
    /**Get the optional field `pkg`.

This child has type `(pkg)?` ([`symbols::LParenpkgRParen`])*/
    #[inline]
    pub fn pkg(
        &self,
    ) -> ::std::option::Option<
        ::type_sitter::NodeResult<'tree, symbols::LParenpkgRParen<'tree>>,
    > {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("pkg")
            .map(
                <symbols::LParenpkgRParen<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Pub<'tree> {
    type WithLifetime<'a> = Pub<'a>;
    const KIND: &'static str = "pub";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "pub" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "pub");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `query_definition`

This node has these fields:

- `grammar`: `fqmn` ([`Fqmn`])
- `name`: `identifier` ([`Identifier`])
- `value`: `query_literal` ([`QueryLiteral`])

And an optional additional named child of type `pub?` ([`Pub`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct QueryDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> QueryDefinition<'tree> {
    /**Get the field `grammar`.

This child has type `fqmn` ([`Fqmn`])*/
    #[inline]
    pub fn grammar(&self) -> ::type_sitter::NodeResult<'tree, Fqmn<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("grammar")
            .map(<Fqmn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `value`.

This child has type `query_literal` ([`QueryLiteral`])*/
    #[inline]
    pub fn value(&self) -> ::type_sitter::NodeResult<'tree, QueryLiteral<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("value")
            .map(<QueryLiteral<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's only non-field not-extra named child, if it has one.

This child has type `pub?` ([`Pub`])*/
    #[inline]
    pub fn r#pub(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, Pub<'tree>>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .filter(|i| {
                ::type_sitter::Node::raw(self)
                    .field_name_for_named_child(*i as _)
                    .is_none()
            })
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<Pub<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for QueryDefinition<'tree> {
    type WithLifetime<'a> = QueryDefinition<'a>;
    const KIND: &'static str = "query_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "query_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "query_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `query_literal`

This node has these fields:

- `content`: `raw_content?` ([`RawContent`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct QueryLiteral<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> QueryLiteral<'tree> {
    /**Get the optional field `content`.

This child has type `raw_content?` ([`RawContent`])*/
    #[inline]
    pub fn content(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, RawContent<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("content")
            .map(<RawContent<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for QueryLiteral<'tree> {
    type WithLifetime<'a> = QueryLiteral<'a>;
    const KIND: &'static str = "query_literal";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "query_literal" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "query_literal");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `range`

This node has these fields:

- `end`: `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}?` ([`Fqmn`] | [`InExpression`] | [`It`] | [`Number`] | [`OperatorIdentifier`] | [`ParenthesizedExpression`] | [`String`])
- `start`: `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}` ([`Fqmn`] | [`InExpression`] | [`It`] | [`Number`] | [`OperatorIdentifier`] | [`ParenthesizedExpression`] | [`String`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Range<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Range<'tree> {
    /**Get the optional field `end`.

This child has type `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}?`:

- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]
*/
    #[inline]
    pub fn end(
        &self,
    ) -> ::std::option::Option<
        ::type_sitter::NodeResult<
            'tree,
            anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
                'tree,
            >,
        >,
    > {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("end")
            .map(
                <anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
    /**Get the field `start`.

This child has type `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}`:

- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]
*/
    #[inline]
    pub fn start(
        &self,
    ) -> ::type_sitter::NodeResult<
        'tree,
        anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
            'tree,
        >,
    > {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("start")
            .map(
                <anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Range<'tree> {
    type WithLifetime<'a> = Range<'a>;
    const KIND: &'static str = "range";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "range" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "range");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `raw_content`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct RawContent<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> RawContent<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for RawContent<'tree> {
    type WithLifetime<'a> = RawContent<'a>;
    const KIND: &'static str = "raw_content";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "raw_content" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "raw_content");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `refinement`

This node has named children of type `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}*`:

- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Refinement<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Refinement<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Refinement<'tree> {
    type Child = anon_unions::Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Refinement<'tree> {
    type WithLifetime<'a> = Refinement<'a>;
    const KIND: &'static str = "refinement";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "refinement" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "refinement");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `relation`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Relation<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Relation<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Relation<'tree> {
    type WithLifetime<'a> = Relation<'a>;
    const KIND: &'static str = "relation";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "relation" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "relation");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `source_file`

This node has named children of type `{edge_definition | extern_definition | fact_definition | import_definition | node_definition | query_definition | type_declaration}*`:

- [`EdgeDefinition`]
- [`ExternDefinition`]
- [`FactDefinition`]
- [`ImportDefinition`]
- [`NodeDefinition`]
- [`QueryDefinition`]
- [`TypeDeclaration`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct SourceFile<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> SourceFile<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for SourceFile<'tree> {
    type Child = anon_unions::Anon314737192860065104467245150540293684006<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for SourceFile<'tree> {
    type WithLifetime<'a> = SourceFile<'a>;
    const KIND: &'static str = "source_file";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "source_file" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "source_file");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `string`

This node has no named children
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct String<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> String<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for String<'tree> {
    type WithLifetime<'a> = String<'a>;
    const KIND: &'static str = "string";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "string" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "string");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_annotation`

This node has named children of type `{refinement | type_annotation | type_application | type_identifier}+`:

- [`Refinement`]
- [`TypeAnnotation`]
- [`TypeApplication`]
- [`TypeIdentifier`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeAnnotation<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeAnnotation<'tree> {}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for TypeAnnotation<'tree> {
    type Child = anon_unions::Refinement_TypeAnnotation_TypeApplication_TypeIdentifier<
        'tree,
    >;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeAnnotation<'tree> {
    type WithLifetime<'a> = TypeAnnotation<'a>;
    const KIND: &'static str = "type_annotation";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_annotation" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_annotation");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_application`

This node has these fields:

- `argument`: `{( | ) | type_annotation | type_identifier}+` ([`symbols::LParen`] | [`symbols::RParen`] | [`TypeAnnotation`] | [`TypeIdentifier`])
- `constructor`: `type_identifier` ([`TypeIdentifier`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeApplication<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeApplication<'tree> {
    /**Get the children of field `argument`.

These children have type `{( | ) | type_annotation | type_identifier}+`:

- [`symbols::LParen`]
- [`symbols::RParen`]
- [`TypeAnnotation`]
- [`TypeIdentifier`]
*/
    /**

This is guaranteed to return at least one child.*/
    #[inline]
    pub fn arguments<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<
            'tree,
            anon_unions::LParen_RParen_TypeAnnotation_TypeIdentifier<'tree>,
        >,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .children_by_field_name("argument", &mut c.0)
            .map(
                <anon_unions::LParen_RParen_TypeAnnotation_TypeIdentifier<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
    /**Get the field `constructor`.

This child has type `type_identifier` ([`TypeIdentifier`])*/
    #[inline]
    pub fn constructor(
        &self,
    ) -> ::type_sitter::NodeResult<'tree, TypeIdentifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("constructor")
            .map(<TypeIdentifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeApplication<'tree> {
    type WithLifetime<'a> = TypeApplication<'a>;
    const KIND: &'static str = "type_application";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_application" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_application");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_declaration`

This node has these fields:

- `body`: `type_definition` ([`TypeDefinition`])
- `name`: `identifier` ([`Identifier`])

And additional named children of type `{attribute | pub}*`:

- [`Attribute`]
- [`Pub`]

*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeDeclaration<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeDeclaration<'tree> {
    /**Get the field `body`.

This child has type `type_definition` ([`TypeDefinition`])*/
    #[inline]
    pub fn body(&self) -> ::type_sitter::NodeResult<'tree, TypeDefinition<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("body")
            .map(<TypeDefinition<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the node's non-field not-extra named children.

These children have type `{attribute | pub}*`:

- [`Attribute`]
- [`Pub`]
*/
    #[inline]
    pub fn others<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, anon_unions::Attribute_Pub<'tree>>,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(
                <anon_unions::Attribute_Pub<
                    'tree,
                > as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeDeclaration<'tree> {
    type WithLifetime<'a> = TypeDeclaration<'a>;
    const KIND: &'static str = "type_declaration";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_declaration" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_declaration");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_definition`

This node has these fields:

- `type`: `type_annotation?` ([`TypeAnnotation`])

And additional named children of type `type_field_definition*` ([`TypeFieldDefinition`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeDefinition<'tree> {
    /**Get the optional field `type`.

This child has type `type_annotation?` ([`TypeAnnotation`])*/
    #[inline]
    pub fn r#type(
        &self,
    ) -> ::std::option::Option<::type_sitter::NodeResult<'tree, TypeAnnotation<'tree>>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("type")
            .map(<TypeAnnotation<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
    /**Get the node's non-field not-extra named children.

These children have type `type_field_definition*` ([`TypeFieldDefinition`])*/
    #[inline]
    pub fn type_field_definitions<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, TypeFieldDefinition<'tree>>,
    > + 'a {
        {
            let me = *::type_sitter::Node::raw(self);
            ::type_sitter::Node::raw(self)
                .named_children(&mut c.0)
                .enumerate()
                .filter(move |(i, n)| {
                    !n.is_extra() && me.field_name_for_named_child(*i as _).is_none()
                })
                .map(|(_, n)| n)
        }
            .map(
                <TypeFieldDefinition<'tree> as ::type_sitter::Node<'tree>>::try_from_raw,
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeDefinition<'tree> {
    type WithLifetime<'a> = TypeDefinition<'a>;
    const KIND: &'static str = "type_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_field_definition`

This node has these fields:

- `name`: `identifier` ([`Identifier`])
- `type`: `type_definition` ([`TypeDefinition`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeFieldDefinition<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeFieldDefinition<'tree> {
    /**Get the field `name`.

This child has type `identifier` ([`Identifier`])*/
    #[inline]
    pub fn name(&self) -> ::type_sitter::NodeResult<'tree, Identifier<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("name")
            .map(<Identifier<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
    /**Get the field `type`.

This child has type `type_definition` ([`TypeDefinition`])*/
    #[inline]
    pub fn r#type(&self) -> ::type_sitter::NodeResult<'tree, TypeDefinition<'tree>> {
        ::type_sitter::Node::raw(self)
            .child_by_field_name("type")
            .map(<TypeDefinition<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeFieldDefinition<'tree> {
    type WithLifetime<'a> = TypeFieldDefinition<'a>;
    const KIND: &'static str = "type_field_definition";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_field_definition" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_field_definition");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `type_identifier`

This node has a named child of type `fqmn` ([`Fqmn`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct TypeIdentifier<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> TypeIdentifier<'tree> {
    /**Get the node's only not-extra named child.

This child has type `fqmn` ([`Fqmn`])*/
    #[inline]
    pub fn fqmn(&self) -> ::type_sitter::NodeResult<'tree, Fqmn<'tree>> {
        (0..::type_sitter::Node::raw(self).named_child_count())
            .map(|i| ::type_sitter::Node::raw(self).named_child(i).unwrap())
            .filter(|n| !n.is_extra())
            .next()
            .map(<Fqmn<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
            .expect(
                "required child not present, there should at least be a MISSING node in its place",
            )
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChild<'tree> for TypeIdentifier<'tree> {
    type Child = Fqmn<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for TypeIdentifier<'tree> {
    type WithLifetime<'a> = TypeIdentifier<'a>;
    const KIND: &'static str = "type_identifier";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "type_identifier" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "type_identifier");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
/**Typed node `variable`

This node has named children of type `property_access*` ([`PropertyAccess`])
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
#[allow(non_camel_case_types)]
pub struct Variable<'tree>(::type_sitter::raw::Node<'tree>);
#[automatically_derived]
#[allow(unused)]
impl<'tree> Variable<'tree> {
    /**Get the node's not-extra named children.

These children have type `property_access*` ([`PropertyAccess`])*/
    #[inline]
    pub fn property_accesss<'a>(
        &self,
        c: &'a mut ::type_sitter::TreeCursor<'tree>,
    ) -> impl ::std::iter::Iterator<
        Item = ::type_sitter::NodeResult<'tree, PropertyAccess<'tree>>,
    > + 'a {
        ::type_sitter::Node::raw(self)
            .named_children(&mut c.0)
            .filter(|n| !n.is_extra())
            .map(<PropertyAccess<'tree> as ::type_sitter::Node<'tree>>::try_from_raw)
    }
}
#[automatically_derived]
impl<'tree> ::type_sitter::HasChildren<'tree> for Variable<'tree> {
    type Child = PropertyAccess<'tree>;
}
#[automatically_derived]
impl<'tree> ::type_sitter::Node<'tree> for Variable<'tree> {
    type WithLifetime<'a> = Variable<'a>;
    const KIND: &'static str = "variable";
    #[inline]
    fn try_from_raw(
        node: ::type_sitter::raw::Node<'tree>,
    ) -> ::type_sitter::NodeResult<'tree, Self> {
        if node.kind() == "variable" {
            Ok(Self(node))
        } else {
            Err(::type_sitter::IncorrectKind::new::<Self>(node))
        }
    }
    #[inline]
    unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
        debug_assert_eq!(node.kind(), "variable");
        Self(node)
    }
    #[inline]
    fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
        &self.0
    }
    #[inline]
    fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
        &mut self.0
    }
    #[inline]
    fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
        self.0
    }
}
pub mod unnamed {
    #[allow(unused_imports)]
    use super::*;
    /**Typed node `edge`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Edge<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Edge<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Edge<'tree> {
        type WithLifetime<'a> = Edge<'a>;
        const KIND: &'static str = "edge";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "edge" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "edge");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `extern`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Extern<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Extern<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Extern<'tree> {
        type WithLifetime<'a> = Extern<'a>;
        const KIND: &'static str = "extern";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "extern" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "extern");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `fact`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Fact<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Fact<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Fact<'tree> {
        type WithLifetime<'a> = Fact<'a>;
        const KIND: &'static str = "fact";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "fact" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "fact");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `import`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Import<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Import<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Import<'tree> {
        type WithLifetime<'a> = Import<'a>;
        const KIND: &'static str = "import";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "import" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "import");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `in`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct In<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> In<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for In<'tree> {
        type WithLifetime<'a> = In<'a>;
        const KIND: &'static str = "in";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "in" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "in");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `it`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct It<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> It<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for It<'tree> {
        type WithLifetime<'a> = It<'a>;
        const KIND: &'static str = "it";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "it" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "it");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `match`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Match<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Match<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Match<'tree> {
        type WithLifetime<'a> = Match<'a>;
        const KIND: &'static str = "match";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "match" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "match");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `node`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Node<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Node<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Node<'tree> {
        type WithLifetime<'a> = Node<'a>;
        const KIND: &'static str = "node";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "node" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "node");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `operator`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Operator<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Operator<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Operator<'tree> {
        type WithLifetime<'a> = Operator<'a>;
        const KIND: &'static str = "operator";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "operator" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "operator");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `pub`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Pub<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Pub<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Pub<'tree> {
        type WithLifetime<'a> = Pub<'a>;
        const KIND: &'static str = "pub";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "pub" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "pub");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `query`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Query<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Query<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Query<'tree> {
        type WithLifetime<'a> = Query<'a>;
        const KIND: &'static str = "query";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "query" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "query");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `type`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Type<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Type<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Type<'tree> {
        type WithLifetime<'a> = Type<'a>;
        const KIND: &'static str = "type";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "type" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "type");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `where`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Where<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Where<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Where<'tree> {
        type WithLifetime<'a> = Where<'a>;
        const KIND: &'static str = "where";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "where" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "where");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
}
pub mod symbols {
    #[allow(unused_imports)]
    use super::*;
    /**Typed node `#`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Hash<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Hash<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Hash<'tree> {
        type WithLifetime<'a> = Hash<'a>;
        const KIND: &'static str = "#";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "#" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "#");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `(`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LParen<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LParen<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LParen<'tree> {
        type WithLifetime<'a> = LParen<'a>;
        const KIND: &'static str = "(";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "(" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "(");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `(pkg)`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LParenpkgRParen<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LParenpkgRParen<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LParenpkgRParen<'tree> {
        type WithLifetime<'a> = LParenpkgRParen<'a>;
        const KIND: &'static str = "(pkg)";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "(pkg)" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "(pkg)");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `)`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RParen<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RParen<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RParen<'tree> {
        type WithLifetime<'a> = RParen<'a>;
        const KIND: &'static str = ")";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ")" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ")");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `,`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Comma<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Comma<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Comma<'tree> {
        type WithLifetime<'a> = Comma<'a>;
        const KIND: &'static str = ",";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "," {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ",");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `->`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct SubGt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> SubGt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for SubGt<'tree> {
        type WithLifetime<'a> = SubGt<'a>;
        const KIND: &'static str = "->";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "->" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "->");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `.`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Dot<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Dot<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Dot<'tree> {
        type WithLifetime<'a> = Dot<'a>;
        const KIND: &'static str = ".";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "." {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ".");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `..`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct DotDot<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> DotDot<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for DotDot<'tree> {
        type WithLifetime<'a> = DotDot<'a>;
        const KIND: &'static str = "..";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ".." {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "..");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `:`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Colon<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Colon<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Colon<'tree> {
        type WithLifetime<'a> = Colon<'a>;
        const KIND: &'static str = ":";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == ":" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), ":");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `<-`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LtSub<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LtSub<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LtSub<'tree> {
        type WithLifetime<'a> = LtSub<'a>;
        const KIND: &'static str = "<-";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "<-" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "<-");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `<->`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LtSubGt<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LtSubGt<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LtSubGt<'tree> {
        type WithLifetime<'a> = LtSubGt<'a>;
        const KIND: &'static str = "<->";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "<->" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "<->");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `=`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Eq<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Eq<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Eq<'tree> {
        type WithLifetime<'a> = Eq<'a>;
        const KIND: &'static str = "=";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "=" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "=");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `?`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Question<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Question<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Question<'tree> {
        type WithLifetime<'a> = Question<'a>;
        const KIND: &'static str = "?";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "?" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "?");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `[`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LBracket<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LBracket<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LBracket<'tree> {
        type WithLifetime<'a> = LBracket<'a>;
        const KIND: &'static str = "[";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "[" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "[");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `]`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RBracket<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RBracket<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RBracket<'tree> {
        type WithLifetime<'a> = RBracket<'a>;
        const KIND: &'static str = "]";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "]" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "]");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node ```

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct Backtick<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Backtick<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Backtick<'tree> {
        type WithLifetime<'a> = Backtick<'a>;
        const KIND: &'static str = "`";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "`" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "`");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `{`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct LBrace<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LBrace<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for LBrace<'tree> {
        type WithLifetime<'a> = LBrace<'a>;
        const KIND: &'static str = "{";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "{" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "{");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
    /**Typed node `}`

This node has no named children
*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[repr(transparent)]
    #[allow(non_camel_case_types)]
    pub struct RBrace<'tree>(::type_sitter::raw::Node<'tree>);
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> RBrace<'tree> {}
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for RBrace<'tree> {
        type WithLifetime<'a> = RBrace<'a>;
        const KIND: &'static str = "}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            if node.kind() == "}" {
                Ok(Self(node))
            } else {
                Err(::type_sitter::IncorrectKind::new::<Self>(node))
            }
        }
        #[inline]
        unsafe fn from_raw_unchecked(node: ::type_sitter::raw::Node<'tree>) -> Self {
            debug_assert_eq!(node.kind(), "}");
            Self(node)
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            &self.0
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            &mut self.0
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            self.0
        }
    }
}
pub mod anon_unions {
    #[allow(unused_imports)]
    use super::*;
    /**One of `{edge_definition | extern_definition | fact_definition | import_definition | node_definition | query_definition | type_declaration}`:
- [`EdgeDefinition`]
- [`ExternDefinition`]
- [`FactDefinition`]
- [`ImportDefinition`]
- [`NodeDefinition`]
- [`QueryDefinition`]
- [`TypeDeclaration`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Anon314737192860065104467245150540293684006<'tree> {
        EdgeDefinition(EdgeDefinition<'tree>),
        ExternDefinition(ExternDefinition<'tree>),
        FactDefinition(FactDefinition<'tree>),
        ImportDefinition(ImportDefinition<'tree>),
        NodeDefinition(NodeDefinition<'tree>),
        QueryDefinition(QueryDefinition<'tree>),
        TypeDeclaration(TypeDeclaration<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Anon314737192860065104467245150540293684006<'tree> {
        ///Returns the node if it is of type `edge_definition` ([`EdgeDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_edge_definition(self) -> ::std::option::Option<EdgeDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::EdgeDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `extern_definition` ([`ExternDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_extern_definition(
            self,
        ) -> ::std::option::Option<ExternDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ExternDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `fact_definition` ([`FactDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_fact_definition(self) -> ::std::option::Option<FactDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::FactDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `import_definition` ([`ImportDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_import_definition(
            self,
        ) -> ::std::option::Option<ImportDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ImportDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `node_definition` ([`NodeDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_node_definition(self) -> ::std::option::Option<NodeDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::NodeDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `query_definition` ([`QueryDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_query_definition(
            self,
        ) -> ::std::option::Option<QueryDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::QueryDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_declaration` ([`TypeDeclaration`]), otherwise returns `None`
        #[inline]
        pub fn as_type_declaration(
            self,
        ) -> ::std::option::Option<TypeDeclaration<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeDeclaration(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for Anon314737192860065104467245150540293684006<'tree> {
        type WithLifetime<'a> = Anon314737192860065104467245150540293684006<'a>;
        const KIND: &'static str = "{edge_definition | extern_definition | fact_definition | import_definition | node_definition | query_definition | type_declaration}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "edge_definition" => {
                    Ok(unsafe {
                        Self::EdgeDefinition(
                            <EdgeDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "extern_definition" => {
                    Ok(unsafe {
                        Self::ExternDefinition(
                            <ExternDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "fact_definition" => {
                    Ok(unsafe {
                        Self::FactDefinition(
                            <FactDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "import_definition" => {
                    Ok(unsafe {
                        Self::ImportDefinition(
                            <ImportDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "node_definition" => {
                    Ok(unsafe {
                        Self::NodeDefinition(
                            <NodeDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "query_definition" => {
                    Ok(unsafe {
                        Self::QueryDefinition(
                            <QueryDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_declaration" => {
                    Ok(unsafe {
                        Self::TypeDeclaration(
                            <TypeDeclaration<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::EdgeDefinition(x) => ::type_sitter::Node::raw(x),
                Self::ExternDefinition(x) => ::type_sitter::Node::raw(x),
                Self::FactDefinition(x) => ::type_sitter::Node::raw(x),
                Self::ImportDefinition(x) => ::type_sitter::Node::raw(x),
                Self::NodeDefinition(x) => ::type_sitter::Node::raw(x),
                Self::QueryDefinition(x) => ::type_sitter::Node::raw(x),
                Self::TypeDeclaration(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::EdgeDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::ExternDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::FactDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::ImportDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::NodeDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::QueryDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeDeclaration(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::EdgeDefinition(x) => x.into_raw(),
                Self::ExternDefinition(x) => x.into_raw(),
                Self::FactDefinition(x) => x.into_raw(),
                Self::ImportDefinition(x) => x.into_raw(),
                Self::NodeDefinition(x) => x.into_raw(),
                Self::QueryDefinition(x) => x.into_raw(),
                Self::TypeDeclaration(x) => x.into_raw(),
            }
        }
    }
    /**One of `{attribute | fact_field_definition | pub}`:
- [`Attribute`]
- [`FactFieldDefinition`]
- [`Pub`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Attribute_FactFieldDefinition_Pub<'tree> {
        Attribute(Attribute<'tree>),
        FactFieldDefinition(FactFieldDefinition<'tree>),
        Pub(Pub<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Attribute_FactFieldDefinition_Pub<'tree> {
        ///Returns the node if it is of type `attribute` ([`Attribute`]), otherwise returns `None`
        #[inline]
        pub fn as_attribute(self) -> ::std::option::Option<Attribute<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Attribute(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `fact_field_definition` ([`FactFieldDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_fact_field_definition(
            self,
        ) -> ::std::option::Option<FactFieldDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::FactFieldDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `pub` ([`Pub`]), otherwise returns `None`
        #[inline]
        pub fn as_pub(self) -> ::std::option::Option<Pub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Pub(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Attribute_FactFieldDefinition_Pub<'tree> {
        type WithLifetime<'a> = Attribute_FactFieldDefinition_Pub<'a>;
        const KIND: &'static str = "{attribute | fact_field_definition | pub}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "attribute" => {
                    Ok(unsafe {
                        Self::Attribute(
                            <Attribute<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "fact_field_definition" => {
                    Ok(unsafe {
                        Self::FactFieldDefinition(
                            <FactFieldDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "pub" => {
                    Ok(unsafe {
                        Self::Pub(
                            <Pub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => ::type_sitter::Node::raw(x),
                Self::FactFieldDefinition(x) => ::type_sitter::Node::raw(x),
                Self::Pub(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => ::type_sitter::Node::raw_mut(x),
                Self::FactFieldDefinition(x) => ::type_sitter::Node::raw_mut(x),
                Self::Pub(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => x.into_raw(),
                Self::FactFieldDefinition(x) => x.into_raw(),
                Self::Pub(x) => x.into_raw(),
            }
        }
    }
    /**One of `{attribute | pub}`:
- [`Attribute`]
- [`Pub`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Attribute_Pub<'tree> {
        Attribute(Attribute<'tree>),
        Pub(Pub<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Attribute_Pub<'tree> {
        ///Returns the node if it is of type `attribute` ([`Attribute`]), otherwise returns `None`
        #[inline]
        pub fn as_attribute(self) -> ::std::option::Option<Attribute<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Attribute(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `pub` ([`Pub`]), otherwise returns `None`
        #[inline]
        pub fn as_pub(self) -> ::std::option::Option<Pub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Pub(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Attribute_Pub<'tree> {
        type WithLifetime<'a> = Attribute_Pub<'a>;
        const KIND: &'static str = "{attribute | pub}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "attribute" => {
                    Ok(unsafe {
                        Self::Attribute(
                            <Attribute<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "pub" => {
                    Ok(unsafe {
                        Self::Pub(
                            <Pub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => ::type_sitter::Node::raw(x),
                Self::Pub(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => ::type_sitter::Node::raw_mut(x),
                Self::Pub(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Attribute(x) => x.into_raw(),
                Self::Pub(x) => x.into_raw(),
            }
        }
    }
    /**One of `{block | pub}`:
- [`Block`]
- [`Pub`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Block_Pub<'tree> {
        Block(Block<'tree>),
        Pub(Pub<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Block_Pub<'tree> {
        ///Returns the node if it is of type `block` ([`Block`]), otherwise returns `None`
        #[inline]
        pub fn as_block(self) -> ::std::option::Option<Block<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Block(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `pub` ([`Pub`]), otherwise returns `None`
        #[inline]
        pub fn as_pub(self) -> ::std::option::Option<Pub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Pub(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Block_Pub<'tree> {
        type WithLifetime<'a> = Block_Pub<'a>;
        const KIND: &'static str = "{block | pub}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "block" => {
                    Ok(unsafe {
                        Self::Block(
                            <Block<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "pub" => {
                    Ok(unsafe {
                        Self::Pub(
                            <Pub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => ::type_sitter::Node::raw(x),
                Self::Pub(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => ::type_sitter::Node::raw_mut(x),
                Self::Pub(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Block(x) => x.into_raw(),
                Self::Pub(x) => x.into_raw(),
            }
        }
    }
    /**One of `{capture_block | variable}`:
- [`CaptureBlock`]
- [`Variable`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum CaptureBlock_Variable<'tree> {
        CaptureBlock(CaptureBlock<'tree>),
        Variable(Variable<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> CaptureBlock_Variable<'tree> {
        ///Returns the node if it is of type `capture_block` ([`CaptureBlock`]), otherwise returns `None`
        #[inline]
        pub fn as_capture_block(self) -> ::std::option::Option<CaptureBlock<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::CaptureBlock(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `variable` ([`Variable`]), otherwise returns `None`
        #[inline]
        pub fn as_variable(self) -> ::std::option::Option<Variable<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Variable(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for CaptureBlock_Variable<'tree> {
        type WithLifetime<'a> = CaptureBlock_Variable<'a>;
        const KIND: &'static str = "{capture_block | variable}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "capture_block" => {
                    Ok(unsafe {
                        Self::CaptureBlock(
                            <CaptureBlock<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "variable" => {
                    Ok(unsafe {
                        Self::Variable(
                            <Variable<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::CaptureBlock(x) => ::type_sitter::Node::raw(x),
                Self::Variable(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::CaptureBlock(x) => ::type_sitter::Node::raw_mut(x),
                Self::Variable(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::CaptureBlock(x) => x.into_raw(),
                Self::Variable(x) => x.into_raw(),
            }
        }
    }
    /**One of `{extern_def_arg | extern_return | identifier | operator_identifier}`:
- [`ExternDefArg`]
- [`ExternReturn`]
- [`Identifier`]
- [`OperatorIdentifier`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum ExternDefArg_ExternReturn_Identifier_OperatorIdentifier<'tree> {
        ExternDefArg(ExternDefArg<'tree>),
        ExternReturn(ExternReturn<'tree>),
        Identifier(Identifier<'tree>),
        OperatorIdentifier(OperatorIdentifier<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> ExternDefArg_ExternReturn_Identifier_OperatorIdentifier<'tree> {
        ///Returns the node if it is of type `extern_def_arg` ([`ExternDefArg`]), otherwise returns `None`
        #[inline]
        pub fn as_extern_def_arg(self) -> ::std::option::Option<ExternDefArg<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ExternDefArg(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `extern_return` ([`ExternReturn`]), otherwise returns `None`
        #[inline]
        pub fn as_extern_return(self) -> ::std::option::Option<ExternReturn<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ExternReturn(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `identifier` ([`Identifier`]), otherwise returns `None`
        #[inline]
        pub fn as_identifier(self) -> ::std::option::Option<Identifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Identifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `operator_identifier` ([`OperatorIdentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_operator_identifier(
            self,
        ) -> ::std::option::Option<OperatorIdentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::OperatorIdentifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for ExternDefArg_ExternReturn_Identifier_OperatorIdentifier<'tree> {
        type WithLifetime<'a> = ExternDefArg_ExternReturn_Identifier_OperatorIdentifier<
            'a,
        >;
        const KIND: &'static str = "{extern_def_arg | extern_return | identifier | operator_identifier}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "extern_def_arg" => {
                    Ok(unsafe {
                        Self::ExternDefArg(
                            <ExternDefArg<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "extern_return" => {
                    Ok(unsafe {
                        Self::ExternReturn(
                            <ExternReturn<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "identifier" => {
                    Ok(unsafe {
                        Self::Identifier(
                            <Identifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "operator_identifier" => {
                    Ok(unsafe {
                        Self::OperatorIdentifier(
                            <OperatorIdentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::ExternDefArg(x) => ::type_sitter::Node::raw(x),
                Self::ExternReturn(x) => ::type_sitter::Node::raw(x),
                Self::Identifier(x) => ::type_sitter::Node::raw(x),
                Self::OperatorIdentifier(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::ExternDefArg(x) => ::type_sitter::Node::raw_mut(x),
                Self::ExternReturn(x) => ::type_sitter::Node::raw_mut(x),
                Self::Identifier(x) => ::type_sitter::Node::raw_mut(x),
                Self::OperatorIdentifier(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::ExternDefArg(x) => x.into_raw(),
                Self::ExternReturn(x) => x.into_raw(),
                Self::Identifier(x) => x.into_raw(),
                Self::OperatorIdentifier(x) => x.into_raw(),
            }
        }
    }
    /**One of `{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}`:
- [`Fqmn`]
- [`InExpression`]
- [`It`]
- [`Number`]
- [`OperatorIdentifier`]
- [`ParenthesizedExpression`]
- [`String`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    > {
        Fqmn(Fqmn<'tree>),
        InExpression(InExpression<'tree>),
        It(It<'tree>),
        Number(Number<'tree>),
        OperatorIdentifier(OperatorIdentifier<'tree>),
        ParenthesizedExpression(ParenthesizedExpression<'tree>),
        String(String<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<
        'tree,
    > Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    > {
        ///Returns the node if it is of type `fqmn` ([`Fqmn`]), otherwise returns `None`
        #[inline]
        pub fn as_fqmn(self) -> ::std::option::Option<Fqmn<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Fqmn(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `in_expression` ([`InExpression`]), otherwise returns `None`
        #[inline]
        pub fn as_in_expression(self) -> ::std::option::Option<InExpression<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::InExpression(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `it` ([`It`]), otherwise returns `None`
        #[inline]
        pub fn as_it(self) -> ::std::option::Option<It<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::It(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `number` ([`Number`]), otherwise returns `None`
        #[inline]
        pub fn as_number(self) -> ::std::option::Option<Number<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Number(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `operator_identifier` ([`OperatorIdentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_operator_identifier(
            self,
        ) -> ::std::option::Option<OperatorIdentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::OperatorIdentifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `parenthesized_expression` ([`ParenthesizedExpression`]), otherwise returns `None`
        #[inline]
        pub fn as_parenthesized_expression(
            self,
        ) -> ::std::option::Option<ParenthesizedExpression<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ParenthesizedExpression(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `string` ([`String`]), otherwise returns `None`
        #[inline]
        pub fn as_string(self) -> ::std::option::Option<String<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::String(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
        'tree,
    > {
        type WithLifetime<'a> = Fqmn_InExpression_It_Number_OperatorIdentifier_ParenthesizedExpression_String<
            'a,
        >;
        const KIND: &'static str = "{fqmn | in_expression | it | number | operator_identifier | parenthesized_expression | string}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "fqmn" => {
                    Ok(unsafe {
                        Self::Fqmn(
                            <Fqmn<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "in_expression" => {
                    Ok(unsafe {
                        Self::InExpression(
                            <InExpression<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "it" => {
                    Ok(unsafe {
                        Self::It(
                            <It<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "number" => {
                    Ok(unsafe {
                        Self::Number(
                            <Number<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "operator_identifier" => {
                    Ok(unsafe {
                        Self::OperatorIdentifier(
                            <OperatorIdentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "parenthesized_expression" => {
                    Ok(unsafe {
                        Self::ParenthesizedExpression(
                            <ParenthesizedExpression<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "string" => {
                    Ok(unsafe {
                        Self::String(
                            <String<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Fqmn(x) => ::type_sitter::Node::raw(x),
                Self::InExpression(x) => ::type_sitter::Node::raw(x),
                Self::It(x) => ::type_sitter::Node::raw(x),
                Self::Number(x) => ::type_sitter::Node::raw(x),
                Self::OperatorIdentifier(x) => ::type_sitter::Node::raw(x),
                Self::ParenthesizedExpression(x) => ::type_sitter::Node::raw(x),
                Self::String(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Fqmn(x) => ::type_sitter::Node::raw_mut(x),
                Self::InExpression(x) => ::type_sitter::Node::raw_mut(x),
                Self::It(x) => ::type_sitter::Node::raw_mut(x),
                Self::Number(x) => ::type_sitter::Node::raw_mut(x),
                Self::OperatorIdentifier(x) => ::type_sitter::Node::raw_mut(x),
                Self::ParenthesizedExpression(x) => ::type_sitter::Node::raw_mut(x),
                Self::String(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Fqmn(x) => x.into_raw(),
                Self::InExpression(x) => x.into_raw(),
                Self::It(x) => x.into_raw(),
                Self::Number(x) => x.into_raw(),
                Self::OperatorIdentifier(x) => x.into_raw(),
                Self::ParenthesizedExpression(x) => x.into_raw(),
                Self::String(x) => x.into_raw(),
            }
        }
    }
    /**One of `{identifier | query_literal}`:
- [`Identifier`]
- [`QueryLiteral`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Identifier_QueryLiteral<'tree> {
        Identifier(Identifier<'tree>),
        QueryLiteral(QueryLiteral<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Identifier_QueryLiteral<'tree> {
        ///Returns the node if it is of type `identifier` ([`Identifier`]), otherwise returns `None`
        #[inline]
        pub fn as_identifier(self) -> ::std::option::Option<Identifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Identifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `query_literal` ([`QueryLiteral`]), otherwise returns `None`
        #[inline]
        pub fn as_query_literal(self) -> ::std::option::Option<QueryLiteral<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::QueryLiteral(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Identifier_QueryLiteral<'tree> {
        type WithLifetime<'a> = Identifier_QueryLiteral<'a>;
        const KIND: &'static str = "{identifier | query_literal}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "identifier" => {
                    Ok(unsafe {
                        Self::Identifier(
                            <Identifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "query_literal" => {
                    Ok(unsafe {
                        Self::QueryLiteral(
                            <QueryLiteral<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => ::type_sitter::Node::raw(x),
                Self::QueryLiteral(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => ::type_sitter::Node::raw_mut(x),
                Self::QueryLiteral(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => x.into_raw(),
                Self::QueryLiteral(x) => x.into_raw(),
            }
        }
    }
    /**One of `{identifier | variable}`:
- [`Identifier`]
- [`Variable`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Identifier_Variable<'tree> {
        Identifier(Identifier<'tree>),
        Variable(Variable<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Identifier_Variable<'tree> {
        ///Returns the node if it is of type `identifier` ([`Identifier`]), otherwise returns `None`
        #[inline]
        pub fn as_identifier(self) -> ::std::option::Option<Identifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Identifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `variable` ([`Variable`]), otherwise returns `None`
        #[inline]
        pub fn as_variable(self) -> ::std::option::Option<Variable<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Variable(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Identifier_Variable<'tree> {
        type WithLifetime<'a> = Identifier_Variable<'a>;
        const KIND: &'static str = "{identifier | variable}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "identifier" => {
                    Ok(unsafe {
                        Self::Identifier(
                            <Identifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "variable" => {
                    Ok(unsafe {
                        Self::Variable(
                            <Variable<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => ::type_sitter::Node::raw(x),
                Self::Variable(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => ::type_sitter::Node::raw_mut(x),
                Self::Variable(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Identifier(x) => x.into_raw(),
                Self::Variable(x) => x.into_raw(),
            }
        }
    }
    /**One of `{( | ) | type_annotation | type_identifier}`:
- [`symbols::LParen`]
- [`symbols::RParen`]
- [`TypeAnnotation`]
- [`TypeIdentifier`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum LParen_RParen_TypeAnnotation_TypeIdentifier<'tree> {
        LParen(symbols::LParen<'tree>),
        RParen(symbols::RParen<'tree>),
        TypeAnnotation(TypeAnnotation<'tree>),
        TypeIdentifier(TypeIdentifier<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> LParen_RParen_TypeAnnotation_TypeIdentifier<'tree> {
        ///Returns the node if it is of type `(` ([`symbols::LParen`]), otherwise returns `None`
        #[inline]
        pub fn as_l_paren(self) -> ::std::option::Option<symbols::LParen<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::LParen(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `)` ([`symbols::RParen`]), otherwise returns `None`
        #[inline]
        pub fn as_r_paren(self) -> ::std::option::Option<symbols::RParen<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::RParen(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_annotation` ([`TypeAnnotation`]), otherwise returns `None`
        #[inline]
        pub fn as_type_annotation(self) -> ::std::option::Option<TypeAnnotation<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeAnnotation(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_identifier` ([`TypeIdentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_type_identifier(self) -> ::std::option::Option<TypeIdentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeIdentifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for LParen_RParen_TypeAnnotation_TypeIdentifier<'tree> {
        type WithLifetime<'a> = LParen_RParen_TypeAnnotation_TypeIdentifier<'a>;
        const KIND: &'static str = "{( | ) | type_annotation | type_identifier}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "(" => {
                    Ok(unsafe {
                        Self::LParen(
                            <symbols::LParen<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                ")" => {
                    Ok(unsafe {
                        Self::RParen(
                            <symbols::RParen<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_annotation" => {
                    Ok(unsafe {
                        Self::TypeAnnotation(
                            <TypeAnnotation<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_identifier" => {
                    Ok(unsafe {
                        Self::TypeIdentifier(
                            <TypeIdentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::LParen(x) => ::type_sitter::Node::raw(x),
                Self::RParen(x) => ::type_sitter::Node::raw(x),
                Self::TypeAnnotation(x) => ::type_sitter::Node::raw(x),
                Self::TypeIdentifier(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::LParen(x) => ::type_sitter::Node::raw_mut(x),
                Self::RParen(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeAnnotation(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeIdentifier(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::LParen(x) => x.into_raw(),
                Self::RParen(x) => x.into_raw(),
                Self::TypeAnnotation(x) => x.into_raw(),
                Self::TypeIdentifier(x) => x.into_raw(),
            }
        }
    }
    /**One of `{list_items | range}`:
- [`ListItems`]
- [`Range`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum ListItems_Range<'tree> {
        ListItems(ListItems<'tree>),
        Range(Range<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> ListItems_Range<'tree> {
        ///Returns the node if it is of type `list_items` ([`ListItems`]), otherwise returns `None`
        #[inline]
        pub fn as_list_items(self) -> ::std::option::Option<ListItems<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::ListItems(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `range` ([`Range`]), otherwise returns `None`
        #[inline]
        pub fn as_range(self) -> ::std::option::Option<Range<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Range(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for ListItems_Range<'tree> {
        type WithLifetime<'a> = ListItems_Range<'a>;
        const KIND: &'static str = "{list_items | range}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "list_items" => {
                    Ok(unsafe {
                        Self::ListItems(
                            <ListItems<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "range" => {
                    Ok(unsafe {
                        Self::Range(
                            <Range<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::ListItems(x) => ::type_sitter::Node::raw(x),
                Self::Range(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::ListItems(x) => ::type_sitter::Node::raw_mut(x),
                Self::Range(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::ListItems(x) => x.into_raw(),
                Self::Range(x) => x.into_raw(),
            }
        }
    }
    /**One of `{match_stmt | query_definition}`:
- [`MatchStmt`]
- [`QueryDefinition`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum MatchStmt_QueryDefinition<'tree> {
        MatchStmt(MatchStmt<'tree>),
        QueryDefinition(QueryDefinition<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> MatchStmt_QueryDefinition<'tree> {
        ///Returns the node if it is of type `match_stmt` ([`MatchStmt`]), otherwise returns `None`
        #[inline]
        pub fn as_match_stmt(self) -> ::std::option::Option<MatchStmt<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::MatchStmt(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `query_definition` ([`QueryDefinition`]), otherwise returns `None`
        #[inline]
        pub fn as_query_definition(
            self,
        ) -> ::std::option::Option<QueryDefinition<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::QueryDefinition(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for MatchStmt_QueryDefinition<'tree> {
        type WithLifetime<'a> = MatchStmt_QueryDefinition<'a>;
        const KIND: &'static str = "{match_stmt | query_definition}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "match_stmt" => {
                    Ok(unsafe {
                        Self::MatchStmt(
                            <MatchStmt<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "query_definition" => {
                    Ok(unsafe {
                        Self::QueryDefinition(
                            <QueryDefinition<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::MatchStmt(x) => ::type_sitter::Node::raw(x),
                Self::QueryDefinition(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::MatchStmt(x) => ::type_sitter::Node::raw_mut(x),
                Self::QueryDefinition(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::MatchStmt(x) => x.into_raw(),
                Self::QueryDefinition(x) => x.into_raw(),
            }
        }
    }
    /**One of `{pub | relation}`:
- [`Pub`]
- [`Relation`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Pub_Relation<'tree> {
        Pub(Pub<'tree>),
        Relation(Relation<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Pub_Relation<'tree> {
        ///Returns the node if it is of type `pub` ([`Pub`]), otherwise returns `None`
        #[inline]
        pub fn as_pub(self) -> ::std::option::Option<Pub<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Pub(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `relation` ([`Relation`]), otherwise returns `None`
        #[inline]
        pub fn as_relation(self) -> ::std::option::Option<Relation<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Relation(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree> for Pub_Relation<'tree> {
        type WithLifetime<'a> = Pub_Relation<'a>;
        const KIND: &'static str = "{pub | relation}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "pub" => {
                    Ok(unsafe {
                        Self::Pub(
                            <Pub<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "relation" => {
                    Ok(unsafe {
                        Self::Relation(
                            <Relation<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Pub(x) => ::type_sitter::Node::raw(x),
                Self::Relation(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Pub(x) => ::type_sitter::Node::raw_mut(x),
                Self::Relation(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Pub(x) => x.into_raw(),
                Self::Relation(x) => x.into_raw(),
            }
        }
    }
    /**One of `{refinement | type_annotation | type_application | type_identifier}`:
- [`Refinement`]
- [`TypeAnnotation`]
- [`TypeApplication`]
- [`TypeIdentifier`]*/
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Refinement_TypeAnnotation_TypeApplication_TypeIdentifier<'tree> {
        Refinement(Refinement<'tree>),
        TypeAnnotation(TypeAnnotation<'tree>),
        TypeApplication(TypeApplication<'tree>),
        TypeIdentifier(TypeIdentifier<'tree>),
    }
    #[automatically_derived]
    #[allow(unused)]
    impl<'tree> Refinement_TypeAnnotation_TypeApplication_TypeIdentifier<'tree> {
        ///Returns the node if it is of type `refinement` ([`Refinement`]), otherwise returns `None`
        #[inline]
        pub fn as_refinement(self) -> ::std::option::Option<Refinement<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::Refinement(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_annotation` ([`TypeAnnotation`]), otherwise returns `None`
        #[inline]
        pub fn as_type_annotation(self) -> ::std::option::Option<TypeAnnotation<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeAnnotation(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_application` ([`TypeApplication`]), otherwise returns `None`
        #[inline]
        pub fn as_type_application(
            self,
        ) -> ::std::option::Option<TypeApplication<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeApplication(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
        ///Returns the node if it is of type `type_identifier` ([`TypeIdentifier`]), otherwise returns `None`
        #[inline]
        pub fn as_type_identifier(self) -> ::std::option::Option<TypeIdentifier<'tree>> {
            #[allow(irrefutable_let_patterns)]
            if let Self::TypeIdentifier(x) = self {
                ::std::option::Option::Some(x)
            } else {
                ::std::option::Option::None
            }
        }
    }
    #[automatically_derived]
    impl<'tree> ::type_sitter::Node<'tree>
    for Refinement_TypeAnnotation_TypeApplication_TypeIdentifier<'tree> {
        type WithLifetime<'a> = Refinement_TypeAnnotation_TypeApplication_TypeIdentifier<
            'a,
        >;
        const KIND: &'static str = "{refinement | type_annotation | type_application | type_identifier}";
        #[inline]
        fn try_from_raw(
            node: ::type_sitter::raw::Node<'tree>,
        ) -> ::type_sitter::NodeResult<'tree, Self> {
            match node.kind() {
                "refinement" => {
                    Ok(unsafe {
                        Self::Refinement(
                            <Refinement<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_annotation" => {
                    Ok(unsafe {
                        Self::TypeAnnotation(
                            <TypeAnnotation<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_application" => {
                    Ok(unsafe {
                        Self::TypeApplication(
                            <TypeApplication<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                "type_identifier" => {
                    Ok(unsafe {
                        Self::TypeIdentifier(
                            <TypeIdentifier<
                                'tree,
                            > as ::type_sitter::Node<'tree>>::from_raw_unchecked(node),
                        )
                    })
                }
                _ => Err(::type_sitter::IncorrectKind::new::<Self>(node)),
            }
        }
        #[inline]
        fn raw(&self) -> &::type_sitter::raw::Node<'tree> {
            match self {
                Self::Refinement(x) => ::type_sitter::Node::raw(x),
                Self::TypeAnnotation(x) => ::type_sitter::Node::raw(x),
                Self::TypeApplication(x) => ::type_sitter::Node::raw(x),
                Self::TypeIdentifier(x) => ::type_sitter::Node::raw(x),
            }
        }
        #[inline]
        fn raw_mut(&mut self) -> &mut ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Refinement(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeAnnotation(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeApplication(x) => ::type_sitter::Node::raw_mut(x),
                Self::TypeIdentifier(x) => ::type_sitter::Node::raw_mut(x),
            }
        }
        #[inline]
        fn into_raw(self) -> ::type_sitter::raw::Node<'tree> {
            match self {
                Self::Refinement(x) => x.into_raw(),
                Self::TypeAnnotation(x) => x.into_raw(),
                Self::TypeApplication(x) => x.into_raw(),
                Self::TypeIdentifier(x) => x.into_raw(),
            }
        }
    }
}
