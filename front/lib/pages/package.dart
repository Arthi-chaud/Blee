import 'package:blee/api/src/client.dart';
import 'package:blee/api/src/models/package.dart';
import 'package:blee/main.g.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:skeletonizer/skeletonizer.dart';

part 'package.g.dart';

@riverpod
Future<Package> activity(ActivityRef ref) async {
  return await APIClient().getPackage("the-corrs-the-corrs-live-at-lansdowne-road");
}

class PackagePage extends StatelessWidget {
  const PackagePage({super.key});

  @override
  Widget build(BuildContext context) {
    return Consumer(
      builder: (context, ref, child) {
        final AsyncValue<Package> data = ref.watch(activityProvider);

        return Center(
            child: Skeletonizer(
                enabled: data.isLoading,
                child: switch (data) {
                  AsyncData(:final value) => Text(value.name),
                  AsyncError(:final error) => Text(error.toString()),
                  _ => Container(),
                }));
      },
    );
  }
}