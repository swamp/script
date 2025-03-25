use seq_map::SeqMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

pub fn print_grid<R, C, V>(map: &Map2<R, C, V>)
where
    R: Display + Eq + Hash + Clone,
    C: Display + Eq + Hash + Clone,
    V: Debug + Clone,
{
    let rows: Vec<_> = map.rows.keys().collect();
    let cols: Vec<_> = map.columns.keys().collect();

    // Print header: leave space for row labels.
    print!("{:>10} ", ""); // empty top-left cell
    for col in &cols {
        print!("{:>10} ", col);
    }
    println!();

    for row in rows {
        print!("{:>10} ", row);
        for col in &cols {
            if let Some(value) = map.get(row, col) {
                print!("{:?} ", value);
            } else {
                print!("{:>10} ", "");
            }
        }
        println!();
    }
}

#[derive(Debug, Clone)]
pub struct Map2<R: Eq + Hash, C: Eq + Hash, V> {
    rows: SeqMap<R, SeqMap<C, V>>,
    columns: SeqMap<C, SeqMap<R, V>>,
}

impl<R, C, V> Map2<R, C, V>
where
    R: Eq + Hash + Clone,
    C: Eq + Hash + Clone,
    V: Clone,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            rows: SeqMap::new(),
            columns: SeqMap::new(),
        }
    }

    pub fn rows(&self) -> &SeqMap<R, SeqMap<C, V>> {
        &self.rows
    }

    pub fn columns(&self) -> &SeqMap<C, SeqMap<R, V>> {
        &self.columns
    }

    /// Returns a reference to the value at the given row and column.
    pub fn get(&self, row: &R, col: &C) -> Option<&V> {
        self.rows.get(row).and_then(|row_map| row_map.get(col))
    }

    /// Gets a reference to the row (i.e. all columns for that row).
    pub fn get_row(&self, row: &R) -> Option<&SeqMap<C, V>> {
        self.rows.get(row)
    }

    /// Gets a reference to the column (i.e. all rows for that column).
    pub fn get_column(&self, col: &C) -> Option<&SeqMap<R, V>> {
        self.columns.get(col)
    }

    /// Inserts a value into the map at the given row and column.
    /// If there was an existing value at that position, it is returned.
    pub fn insert(&mut self, row: R, col: C, value: V) {
        // Insert into the rows map.
        if self.rows.contains_key(&row) {
            self.rows
                .get_mut(&row)
                .unwrap()
                .insert(col.clone(), value.clone())
                .unwrap();
        } else {
            let mut row_map = SeqMap::new();
            row_map.insert(col.clone(), value.clone()).unwrap();
            self.rows.insert(row.clone(), row_map).unwrap();
        };

        // Insert into the columns map.
        if self.columns.contains_key(&col) {
            self.columns
                .get_mut(&col)
                .unwrap()
                .insert(row, value)
                .unwrap();
        } else {
            let mut col_map = SeqMap::new();
            col_map.insert(row, value).unwrap();
            self.columns.insert(col, col_map).unwrap();
        }
    }

    /// Removes the value at the given row and column.
    /// Returns the removed value, if it existed.
    pub fn remove(&mut self, row: &R, col: &C) -> Option<V> {
        // Remove from the rows map.
        let removed = if let Some(row_map) = self.rows.get_mut(row) {
            let removed = row_map.remove(col);
            // Clean up if the row becomes empty.
            if row_map.is_empty() {
                self.rows.remove(row);
            }
            removed
        } else {
            None
        };

        // Remove from the columns map.
        if let Some(col_map) = self.columns.get_mut(col) {
            col_map.remove(row);
            // Clean up if the column becomes empty.
            if col_map.is_empty() {
                self.columns.remove(col);
            }
        }
        removed
    }
}
