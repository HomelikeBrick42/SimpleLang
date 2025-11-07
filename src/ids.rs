use rustc_hash::FxHashMap;
use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{Index, IndexMut},
    sync::atomic::{AtomicUsize, Ordering},
};

pub struct Id<T>(NonZeroUsize, PhantomData<T>);

impl<T> Id<T> {
    fn next() -> Self {
        static NEXT_ID: AtomicUsize = AtomicUsize::new(1);
        if let Some(id) = NonZeroUsize::new(NEXT_ID.fetch_add(1, Ordering::Relaxed)) {
            Self(id, PhantomData)
        } else {
            eprintln!(
                "Internal Compiler Error: NEXT_ID should not have overflowed in any reasonable program"
            );
            std::process::exit(1)
        }
    }
}

impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Id")
            .field("type", &std::any::type_name::<T>())
            .field("id", &self.0)
            .finish()
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

// TODO: optimisation that could be done in the future
// maybe this could be a `Vec<(Id<T>, T)>` that always gets appended to the end
// and a binary search for looking up elements?
pub struct IdMap<T> {
    items: FxHashMap<Id<T>, T>,
}

impl<T> IdMap<T> {
    pub fn new() -> Self {
        Self {
            items: FxHashMap::default(),
        }
    }

    pub fn insert(&mut self, value: T) -> Id<T> {
        self.insert_with(|_| value)
    }

    pub fn insert_with(&mut self, f: impl FnOnce(Id<T>) -> T) -> Id<T> {
        let id = Id::next();
        self.insert_id(id, f(id));
        id
    }

    pub fn insert_id(&mut self, id: Id<T>, value: T) -> Option<T> {
        self.items.insert(id, value)
    }

    pub fn remove(&mut self, id: Id<T>) -> Option<T> {
        self.items.remove(&id)
    }

    pub fn get(&self, id: Id<T>) -> Option<&T> {
        self.items.get(&id)
    }

    pub fn get_mut(&mut self, id: Id<T>) -> Option<&mut T> {
        self.items.get_mut(&id)
    }
}

impl<T> Index<Id<T>> for IdMap<T> {
    type Output = T;

    fn index(&self, id: Id<T>) -> &Self::Output {
        if let Some(value) = self.get(id) {
            value
        } else {
            panic!("{id:?} was not found in IdMap")
        }
    }
}

impl<T> IndexMut<Id<T>> for IdMap<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        if let Some(value) = self.get_mut(id) {
            value
        } else {
            panic!("{id:?} was not found in IdMap")
        }
    }
}

impl<T> Default for IdMap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Debug> Debug for IdMap<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Items<'a, T: Debug> {
            items: &'a FxHashMap<Id<T>, T>,
        }

        impl<T: Debug> Debug for Items<'_, T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_map()
                    .entries(self.items.iter().map(|(id, value)| (id.0, value)))
                    .finish()
            }
        }

        f.debug_struct("IdMap")
            .field("type", &core::any::type_name::<T>())
            .field("items", &Items { items: &self.items })
            .finish()
    }
}
