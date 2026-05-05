use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// Default max examples per key in a FrequencyTable.
pub const DEFAULT_MAX_EXAMPLES: usize = 10;

/// A frequency table keyed by `K` with bounded example lists of type `E`.
///
/// Examples are deduplicated by equality. Each unique example contributes at
/// most once to the example list regardless of how many times it is recorded.
/// The count always increments on every `record` call.
///
/// Serializes as a flat `{"key": {"count": N, "examples": [...]}, ...}`
/// object. The `max_examples` field is runtime behavior, not wire format.
#[derive(Debug, Clone)]
pub struct FrequencyTable<K, E> {
    max_examples: usize,
    entries: BTreeMap<K, FrequencyEntry<E>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrequencyEntry<E> {
    pub count: usize,
    pub examples: Vec<E>,
}

impl<K: Ord, E: Eq> FrequencyTable<K, E> {
    #[must_use]
    pub fn new(max_examples: usize) -> Self {
        Self {
            max_examples,
            entries: BTreeMap::new(),
        }
    }

    /// Records an occurrence of `key` with the given example.
    pub fn record(&mut self, key: K, example: E) {
        let entry = self.entries.entry(key).or_insert_with(|| FrequencyEntry {
            count: 0,
            examples: Vec::new(),
        });
        entry.count += 1;
        if entry.examples.len() < self.max_examples
            && !entry.examples.iter().any(|existing| existing == &example)
        {
            entry.examples.push(example);
        }
    }

    pub fn get(&self, key: &K) -> Option<&FrequencyEntry<E>> {
        self.entries.get(key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &FrequencyEntry<E>)> {
        self.entries.iter()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl<K: Serialize, E: Serialize> Serialize for FrequencyTable<K, E> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.entries.serialize(serializer)
    }
}

impl<'de, K, E> Deserialize<'de> for FrequencyTable<K, E>
where
    K: Deserialize<'de> + Ord,
    E: Deserialize<'de> + Eq,
{
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let entries = BTreeMap::<K, FrequencyEntry<E>>::deserialize(deserializer)?;
        Ok(Self {
            max_examples: DEFAULT_MAX_EXAMPLES,
            entries,
        })
    }
}

impl<K: Ord, E: Eq> Default for FrequencyTable<K, E> {
    fn default() -> Self {
        Self::new(DEFAULT_MAX_EXAMPLES)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_table() {
        let table: FrequencyTable<String, String> = FrequencyTable::new(5);
        assert!(table.is_empty());
        assert_eq!(table.len(), 0);
    }

    #[test]
    fn single_key_multi_example() {
        let mut table: FrequencyTable<String, String> = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 2);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn deduplicates_examples() {
        let mut table: FrequencyTable<String, String> = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 3);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn enforces_max_examples() {
        let mut table: FrequencyTable<String, String> = FrequencyTable::new(2);
        table.record("ruby".into(), "a".into());
        table.record("ruby".into(), "b".into());
        table.record("ruby".into(), "c".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 3);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn iteration_order_is_deterministic() {
        let mut table: FrequencyTable<String, String> = FrequencyTable::new(10);
        table.record("b".into(), "x".into());
        table.record("a".into(), "y".into());
        let keys: Vec<&String> = table.iter().map(|(k, _)| k).collect();
        assert_eq!(keys, vec!["a", "b"]);
    }

    #[test]
    fn serialization_round_trip() {
        let mut table: FrequencyTable<String, String> = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let json = serde_json::to_value(&table).unwrap();
        assert!(json.get("max_examples").is_none());
        let restored: FrequencyTable<String, String> = serde_json::from_value(json).unwrap();
        let entry = restored.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 2);
        assert_eq!(entry.examples, vec!["work1", "work2"]);
    }
}
