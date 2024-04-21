import 'package:blee/api/src/client.dart';
import 'package:blee/api/src/models/package.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:responsive_framework/responsive_framework.dart';

part 'package.g.dart';

@riverpod
Future<Package> activity(ActivityRef ref) async {
  return await APIClient().getPackage("the-corrs-live-at-lansdowne-road");
}

class PackagePage extends StatelessWidget {
  const PackagePage({super.key});

  @override
  Widget build(BuildContext context) {
    return Consumer(
      builder: (context, ref, child) {
        final AsyncValue<Package> package = ref.watch(activityProvider);

        return MaxWidthBox(
          maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
          child: Container(color: Colors.green),
        );
      },
    );
  }
}
