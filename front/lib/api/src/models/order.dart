enum Ordering {
  asc,
  desc;

  @override
  String toString() {
    switch (this) {
      case Ordering.asc:
        return "asc";
      case Ordering.desc:
        return "desc";
    }
  }
}
