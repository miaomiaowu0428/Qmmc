use crate::compile::RawType;


#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub mutable: bool,
    pub initialized: bool,
    pub r#type: RawType,
}
impl VariableSymbol {
    pub fn new(mutable: bool, r#type: RawType) -> Self {
        Self {
            mutable,
            initialized:false,
            r#type,
        }
    }

    pub fn new_mut(r#type: RawType) -> Self {
        Self {
            mutable: true,
            initialized:false,
            r#type,
        }
    }
    pub fn new_immut(r#type: RawType) -> Self {
        Self {
            mutable: false,
            initialized:false,
            r#type,
        }
    }


    pub fn init_as(mutable: bool, r#type: RawType) -> Self {
        Self {
            mutable,
            initialized:true,
            r#type,
        }
    }

    pub fn init_as_mut(r#type: RawType) -> Self {
        Self {
            mutable: true,
            initialized:true,
            r#type,
        }
    }
    pub fn init_as_immut(r#type: RawType) -> Self {
        Self {
            mutable: false,
            initialized:true,
            r#type,
        }
    }
}

