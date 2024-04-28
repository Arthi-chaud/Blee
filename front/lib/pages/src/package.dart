import 'package:blee/api/src/client.dart';
import 'package:blee/api/src/models/package.dart';
import 'package:blee/api/src/models/image.dart' as blee_image;
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:responsive_framework/responsive_framework.dart';

part 'package.g.dart';

@riverpod
Future<Package> activity(ActivityRef ref) async {
  return await APIClient().getPackage("madonna-the-video-collection-93-99");
}

class _PackagePageHeader extends StatelessWidget {
  final String? packageTitle;
  final String? artistName;
  final DateTime? releaseDate;
  final blee_image.Image? poster;
  final bool isCompilation;
  const _PackagePageHeader(
      {super.key,
      required this.packageTitle,
      required this.artistName,
      required this.releaseDate,
      required this.isCompilation,
      required this.poster});

  @override
  Widget build(BuildContext context) {
    var title = Text(
      packageTitle ?? '',
      style: Theme.of(context).textTheme.titleLarge,
    );
    var subtitle = Text(
      artistName ?? '',
      style: Theme.of(context).textTheme.titleMedium,
    );
    var info = Text(
      releaseDate?.year.toString() ?? '',
      style: Theme.of(context).textTheme.labelLarge,
    );
    if (ResponsiveBreakpoints.of(context).smallerThan(BreakpointEnum.sm.name)) {
      return Column(mainAxisSize: MainAxisSize.min, children: [
        Center(
          child: AspectRatio(
            aspectRatio: 16 / 9,
            child: Poster(image: poster),
          ),
        ),
        title,
        subtitle,
        info
      ]);
    }
    return IntrinsicHeight(
      child: Row(
        children: [
          Flexible(child: Poster(image: poster)),
          Flexible(
            flex: 2,
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: [title, subtitle, info],
            ),
          )
        ],
      ),
    );
  }
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
          child: switch (package) {
            AsyncData(:final value) => _PackagePageHeader(
                packageTitle: value.name,
                artistName: value.artistName,
                isCompilation: false,
                releaseDate: DateTime.now(),
                poster: value.poster),
            _ => Container()
          },
        );
      },
    );
  }
}
