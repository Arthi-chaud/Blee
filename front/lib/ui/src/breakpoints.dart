import 'package:responsive_framework/responsive_framework.dart';

enum BreakpointEnum {
  xs,
  sm,
  md,
  lg,
  xl,
}

// Breakpoints values taken from MUI
// https://mui.com/material-ui/customization/breakpoints/
class Breakpoints {
  static getSized(BreakpointEnum bp) {
    switch (bp) {
      case BreakpointEnum.xs:
        return 600;
      case BreakpointEnum.sm:
        return 900;
      case BreakpointEnum.md:
        return 1200;
      case BreakpointEnum.lg:
        return 1536;
      case BreakpointEnum.xl:
        return double.infinity;
    }
  }

  static List<Breakpoint> getList() {
    return [
      Breakpoint(start: 0, end: 600, name: BreakpointEnum.xs.name),
      Breakpoint(start: 601, end: 900, name: BreakpointEnum.sm.name),
      Breakpoint(start: 901, end: 1200, name: BreakpointEnum.md.name),
      Breakpoint(start: 1201, end: 1536, name: BreakpointEnum.lg.name),
      Breakpoint(
          start: 1537, end: double.infinity, name: BreakpointEnum.xl.name),
    ];
  }
}
