use inkwell::basic_block::BasicBlock;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct LoopInfo<'ctx> {
    pub(crate) merge_block: BasicBlock<'ctx>,
    pub(crate) body_block: BasicBlock<'ctx>,
}
pub struct LoopStack<'ctx> {
    stack: RefCell<Vec<LoopInfo<'ctx>>>,
}
impl<'ctx> LoopStack<'ctx> {
    pub fn new() -> Self {
        Self {
            stack: RefCell::new(vec![]),
        }
    }
    pub fn push(&self, loop_info: LoopInfo<'ctx>) {
        self.stack.borrow_mut().push(loop_info);
    }
    pub fn pop(&self) -> Option<LoopInfo<'ctx>> {
        self.stack.borrow_mut().pop()
    }
    pub fn top(&self) -> Option<LoopInfo<'ctx>> {
        self.stack.borrow().last().cloned()
    }
}

pub struct LoopGuard<'a, 'ctx> {
    loop_stack: &'a LoopStack<'ctx>,
}

impl<'a, 'ctx> LoopGuard<'a, 'ctx> {
    pub fn new(
        loop_stack: &'a LoopStack<'ctx>,
        merge_block: BasicBlock<'ctx>,
        body_block: BasicBlock<'ctx>,
    ) -> Self {
        loop_stack.push(LoopInfo {
            merge_block,
            body_block,
        });
        Self { loop_stack }
    }
}

impl<'a, 'ctx> Drop for LoopGuard<'a, 'ctx> {
    fn drop(&mut self) {
        self.loop_stack.pop();
    }
}
