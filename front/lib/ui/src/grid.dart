import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';

// ignore: non_constant_identifier_names
SliverGridDelegate DefaultGridDelegate(BuildContext context) {
  return SliverGridDelegateWithFixedCrossAxisCount(
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
