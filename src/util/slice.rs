/// Divides a slice into a lower slice, the pivot item, and an upper slice.
///
/// - The lower slice contains all items with an index < `position`.
/// - The upper slice contains all items with an index > `position`.
/// - The pivot element is located at `position`.
///
/// # Panics
///
/// Panics if `position >= slice.len()`.
///
/// # Example
///
/// ```ignore
/// let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
/// let (lower, pivot, upper) = split_pivot(&vec, 4);
/// assert_eq!(lower, [0, 1, 2, 3]);
/// assert_eq!(*pivot, 4);
/// assert_eq!(upper, [5, 6, 7, 8, 9]);
/// ```
pub fn split_pivot<'a, T>(slice: &'a [T], position: usize) -> (&'a [T], &'a T, &'a [T]) {
    assert!(position < slice.len());
    let (lower, rest) = slice.split_at(position);
    let (pivot, upper) = rest.split_first().unwrap();
    (lower, pivot, upper)
}

/// Divides a mutable slice into a lower slice, the pivot item, and an upper slice.
///
/// - The lower slice contains all items with an index < `position`.
/// - The upper slice contains all items with an index > `position`.
/// - The pivot element is located at `position`.
///
/// # Panics
///
/// Panics if `position >= slice.len()`.
///
/// # Example
///
/// ```ignore
/// let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
/// let (lower, pivot, upper) = split_pivot_mut(&mut vec, 4);
/// assert_eq!(lower, [0, 1, 2, 3]);
/// assert_eq!(*pivot, 4);
/// assert_eq!(upper, [5, 6, 7, 8, 9]);
/// ```
pub fn split_pivot_mut<'a, T>(
    slice: &'a mut [T],
    position: usize,
) -> (&'a mut [T], &'a mut T, &'a mut [T]) {
    assert!(position < slice.len());
    let (lower, rest) = slice.split_at_mut(position);
    let (pivot, upper) = rest.split_first_mut().unwrap();
    (lower, pivot, upper)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_pivot_all() {
        let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

        for i in 0..vec.len() {
            let (_, _, _) = split_pivot(&vec, i);
        }
    }

    #[test]
    fn test_split_pivot_middle() {
        let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot(&vec, 4);
        assert_eq!(lower, [0, 1, 2, 3]);
        assert_eq!(*pivot, 4);
        assert_eq!(upper, [5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_split_pivot_mut_all() {
        let mut vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

        for i in 0..vec.len() {
            let (_, _, _) = split_pivot_mut(&mut vec, i);
        }
    }

    #[test]
    fn test_split_pivot_mut_middle() {
        let mut vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot_mut(&mut vec, 4);
        assert_eq!(lower, [0, 1, 2, 3]);
        assert_eq!(*pivot, 4);
        assert_eq!(upper, [5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_split_pivot_start() {
        let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot(&vec, 0);
        assert_eq!(lower, []);
        assert_eq!(*pivot, 0);
        assert_eq!(upper, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_split_pivot_mut_start() {
        let mut vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot_mut(&mut vec, 0);
        assert_eq!(lower, []);
        assert_eq!(*pivot, 0);
        assert_eq!(upper, [1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_split_pivot_end() {
        let vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot(&vec, 9);
        assert_eq!(lower, [0, 1, 2, 3, 4, 5, 6, 7, 8]);
        assert_eq!(*pivot, 9);
        assert_eq!(upper, []);
    }

    #[test]
    fn test_split_pivot_mut_end() {
        let mut vec = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let (lower, pivot, upper) = split_pivot_mut(&mut vec, 9);
        assert_eq!(lower, [0, 1, 2, 3, 4, 5, 6, 7, 8]);
        assert_eq!(*pivot, 9);
        assert_eq!(upper, []);
    }

    #[test]
    #[should_panic]
    fn test_split_pivot_empty() {
        let vec = Vec::<u8>::new();
        split_pivot(&vec, 0);
    }

    #[test]
    #[should_panic]
    fn test_split_pivot_mut_empty() {
        let mut vec = Vec::<u8>::new();
        split_pivot_mut(&mut vec, 0);
    }

    #[test]
    #[should_panic]
    fn test_split_pivot_empty_outside() {
        let vec = Vec::<u8>::new();
        let _ = split_pivot(&vec, 1);
    }

    #[test]
    #[should_panic]
    fn test_split_pivot_mut_empty_outside() {
        let mut vec = Vec::<u8>::new();
        let _ = split_pivot_mut(&mut vec, 1);
    }
}
