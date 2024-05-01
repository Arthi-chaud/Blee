import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/api/src/models/image.dart' as blee_image;
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/grid.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:responsive_framework/responsive_framework.dart';
import 'package:skeletonizer/skeletonizer.dart';

import '../../utils/format_duration.dart';

part 'package.g.dart';

@riverpod
Future<Package> getPackage(GetPackageRef ref) async {
  return await APIClient().getPackage("ee1442ec-965b-4d3c-b6a1-4bb6db972a1b");
}

@riverpod
Future<page.Page<Movie>> getMovies(GetMoviesRef ref) async {
  return await APIClient().getMovies("ee1442ec-965b-4d3c-b6a1-4bb6db972a1b");
}

@riverpod
Future<List<Chapter>> getChapters(GetChaptersRef ref, String movieUuid) async {
  return await APIClient().getChapters(movieUuid);
}

@riverpod
Future<page.Page<Extra>> getExtras(GetExtrasRef ref) async {
  return await APIClient().getExtras("ee1442ec-965b-4d3c-b6a1-4bb6db972a1b");
}

class _PackagePageHeader extends StatelessWidget {
  final String? packageTitle;
  final String? artistName;
  final String? artistUuid;
  final DateTime? releaseDate;
  final blee_image.Image? poster;
  final bool isCompilation;
  const _PackagePageHeader(
      {super.key,
      required this.packageTitle,
      required this.artistName,
      required this.artistUuid,
      required this.releaseDate,
      required this.isCompilation,
      required this.poster});

  @override
  Widget build(BuildContext context) {
    var isLoading = packageTitle == null;
    var title = Skeletonizer(
        enabled: isLoading,
        child: Text(
          packageTitle ?? 'No Package Name',
          style: Theme.of(context).textTheme.titleLarge,
        ));
    var subtitle = Skeletonizer(
        enabled: isLoading,
        child: TextButton(
            onPressed: () => context.go('/artists/${artistUuid}'),
            child: Text(
              artistName ?? 'No Artist Name',
              style: Theme.of(context).textTheme.titleMedium,
            )));
    var info = Text(
      releaseDate?.year.toString() ?? '',
      style: Theme.of(context).textTheme.labelLarge,
    );
    paddingForVerticalText(Widget w) => Padding(
          padding: const EdgeInsets.only(left: 14),
          child: w,
        );
    if (ResponsiveBreakpoints.of(context).smallerThan(BreakpointEnum.sm.name)) {
      return Column(mainAxisSize: MainAxisSize.min, children: [
        Center(
          child: AspectRatio(
            aspectRatio: 16 / 9,
            child: Poster(image: poster),
          ),
        ),
        Padding(
          padding: const EdgeInsets.only(top: 16, bottom: 4),
          child: title,
        ),
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
            child: Padding(
              padding: const EdgeInsets.only(left: 8),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  paddingForVerticalText(title),
                  subtitle,
                  paddingForVerticalText(info)
                ],
              ),
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
        final AsyncValue<page.Page<Movie>> movies =
            ref.watch(getMoviesProvider);

        return MaxWidthBox(
            maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
            child: CustomScrollView(
              slivers: [
                SliverList.list(
                  children: [
                    SizedBox.fromSize(size: const Size.fromHeight(16)),
                    _PackagePageHeader(
                        packageTitle: package.asData?.value.name,
                        artistName: package.asData?.value.artistName,
                        artistUuid: package.asData?.value.artistId,
                        isCompilation: package.asData?.value.artistId == null,
                        releaseDate: package.asData?.value.releaseDate,
                        poster: package.asData?.value.poster)
                  ],
                ),
                SliverGrid.builder(
                  itemCount: extras.asData?.value.metadata.count ?? 2,
                  itemBuilder: (context, index) {
                    var item = extras.asData?.value.items[index];
                    return Padding(
                      padding: const EdgeInsets.all(4),
                      child: Tile(
                        title: item?.name,
                        subtitle:
                            item != null ? formatDuration(item.duration) : null,
                        thumbnail: item?.thumbnail,
                        onTap: () {},
                      ),
                    );
                  },
                  gridDelegate: DefaultTileGridDelegate(context),
                ),
              ],
            ));
      },
    );
  }
}
