import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';

// ignore: non_constant_identifier_names
SliverGridDelegate DefaultTileGridDelegate(BuildContext context) {
  return SliverGridDelegateWithFixedCrossAxisCount(
    childAspectRatio: 3.8/3,
    crossAxisSpacing: 4,
    mainAxisSpacing: 4,
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
