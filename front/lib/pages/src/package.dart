import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/package.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/api/src/models/image.dart' as blee_image;
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:responsive_framework/responsive_framework.dart';

import '../../utils/format_duration.dart';

part 'package.g.dart';

@riverpod
Future<Package> getPackage(GetPackageRef ref) async {
  return await APIClient().getPackage("madonna-the-video-collection-93-99");
}

@riverpod
Future<page.Page<Extra>> getExtras(GetExtrasRef ref) async {
  return await APIClient().getExtras("ee1442ec-965b-4d3c-b6a1-4bb6db972a1b");
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
        final AsyncValue<Package> package = ref.watch(getPackageProvider);
        final AsyncValue<page.Page<Extra>> extras =
            ref.watch(getExtrasProvider);

        return MaxWidthBox(
            maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
            child: CustomScrollView(
              slivers: [
                SliverList.list(
                  children: [
                    switch (package) {
                      AsyncData(:final value) => _PackagePageHeader(
                          packageTitle: value.name,
                          artistName: value.artistName,
                          isCompilation: value.artistId == null,
                          releaseDate: value.releaseDate,
                          poster: value.poster),
                      _ => Container()
                    }
                  ],
                ),
                SliverGrid.builder(
                  itemCount: extras.asData?.value.metadata.count ?? 2,
                  itemBuilder: (context, index) {
                    var item = extras.asData?.value.items[index];
                    return Padding(
                      padding: const EdgeInsets.all(2),
                      child: Tile(
                          title: item?.name,
                          subtitle: item != null
                              ? formatDuration(item.duration)
                              : null,
                          thumbnail: item?.thumbnail),
                    );
                  },
                  gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
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
                  ),
                ),
              ],
            ));
      },
    );
  }
}
