
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum Chomping {
    Strip,
    Clip,
    Keep,
}