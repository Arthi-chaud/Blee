import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';

SliverGridDelegate DefaultThumbnailTileGridDelegate(BuildContext context) {
  return SliverGridDelegateWithFixedCrossAxisCount(
    childAspectRatio: 1.25,
    crossAxisSpacing: 8,
    mainAxisSpacing: 8,
    crossAxisCount: ResponsiveBreakpoints.of(context)
            .largerOrEqualTo(BreakpointEnum.xl.name)
        ? 5
        : ResponsiveBreakpoints.of(context)
                .largerOrEqualTo(BreakpointEnum.md.name)
            ? 4
            : ResponsiveBreakpoints.of(context)
                    .largerOrEqualTo(BreakpointEnum.sm.name)
                ? 3
                : 2,
  );
}

SliverGridDelegate DefaultPosterTileGridDelegate(BuildContext context) {
  return SliverGridDelegateWithFixedCrossAxisCount(
    childAspectRatio: 0.55,
    crossAxisSpacing: 8,
    mainAxisSpacing: 8,
    crossAxisCount: ResponsiveBreakpoints.of(context)
            .largerOrEqualTo(BreakpointEnum.xl.name)
        ? 9
        : ResponsiveBreakpoints.of(context)
                .largerOrEqualTo(BreakpointEnum.lg.name)
            ? 7
            : ResponsiveBreakpoints.of(context)
                    .largerOrEqualTo(BreakpointEnum.md.name)
                ? 6
                : ResponsiveBreakpoints.of(context)
                        .largerOrEqualTo(BreakpointEnum.sm.name)
                    ? 5
                    : 3,
  );
}
