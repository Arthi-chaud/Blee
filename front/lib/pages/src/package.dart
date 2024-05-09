import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/api/src/models/image.dart' as blee_image;
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/infinite_scroll.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';
import 'package:riverpod_annotation/riverpod_annotation.dart';
import 'package:skeletonizer/skeletonizer.dart';
import 'package:collection/collection.dart';

import '../../utils/format_duration.dart';
part 'package.g.dart';

//// Package Page

@riverpod
Future<(Package, page.Page<Movie>, page.Page<ExternalId>)> getPackagePageData(
    GetPackagePageDataRef ref, String packageUuid) async {
  final Package package =
      await ref.watch(getPackageProvider(packageUuid).future);
  final page.Page<Movie> movies =
      await ref.watch(getMoviesProvider(packageUuid: packageUuid).future);
  final page.Page<ExternalId> externalId =
      await ref.watch(getPackageExternalIdsProvider(packageUuid).future);
  return (package, movies, externalId);
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
            onPressed: () => context.push('/artists/$artistUuid'),
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

class _PackageDescriptionBox extends StatelessWidget {
  final String? description;
  final bool skeletonize;

  const _PackageDescriptionBox(
      {required this.description, required this.skeletonize});

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        Padding(
          padding: const EdgeInsets.symmetric(horizontal: 8.0, vertical: 4),
          child: skeletonize
              ? Column(
                  children: List.generate(3, (index) => index)
                      .map((i) => Skeletonizer(
                              child: Text(
                            List.generate(200, (index) => ' ').toString(),
                            maxLines: 1,
                          )))
                      .toList())
              : Text(
                  description ?? '',
                  maxLines: 3,
                  overflow: TextOverflow.ellipsis,
                ),
        ),
        Positioned.fill(
            child: ClipRRect(
                borderRadius: BorderRadius.circular(8),
                child: Material(
                    color: Colors.transparent,
                    child: description == null
                        ? Container()
                        : InkWell(
                            onTap: () => _showFullTextDialog(context),
                          )))),
      ],
    );
  }

  Future<void> _showFullTextDialog(BuildContext context) {
    return showDialog(
        context: context,
        builder: (context) {
          return AlertDialog(
            title: const Text('Description'),
            content: Text(description!),
            actions: [
              TextButton(
                  onPressed: () => Navigator.of(context).pop(),
                  child: const Text('Close'))
            ],
          );
        });
  }
}

class PackagePage extends ConsumerWidget {
  final String packageUuid;
  const PackagePage({super.key, required this.packageUuid});

  @override
  Widget build(context, ref) {
    final client = ref.read(apiClientProvider);
    final data = ref.watch(getPackagePageDataProvider(packageUuid));
    final package = data.value?.$1;
    final movies = data.value?.$2;
    final externalIds = data.value?.$3;

    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
        child: CustomScrollView(
          slivers: [
            SliverList.list(
              children: [
                SizedBox.fromSize(size: const Size.fromHeight(16)),
                _PackagePageHeader(
                    key: Key('$packageUuid-header'),
                    packageTitle: package?.name,
                    artistName: package?.artistName,
                    artistUuid: package?.artistId,
                    isCompilation: package?.artistId == null,
                    releaseDate: package?.releaseDate,
                    poster: package?.poster),
                SizedBox.fromSize(size: const Size.fromHeight(8)),
                _PackageDescriptionBox(
                  description: externalIds?.items.firstOrNull?.description,
                  skeletonize: externalIds == null,
                )
              ],
            ),
            SliverToBoxAdapter(
              child: (movies?.metadata.count ?? 1) == 1
                  ? Skeletonizer(
                      enabled: movies == null,
                      child: Padding(
                          padding: const EdgeInsets.only(bottom: 8, top: 8),
                          child: ElevatedButton.icon(
                            icon: const FaIcon(FontAwesomeIcons.play, size: 15),
                            label: const Text('Play'),
                            onPressed: () => context.push(
                                '/player/movie:${movies!.items.first.id}'),
                          )))
                  : Container(),
            ),
            ...(movies?.items.map((movie) {
                  var isOnlyMovie = movies.metadata.count == 1;
                  return ThumbnailTileGridView(
                      key: Key('$movie-chapters'),
                      header: Text(
                        isOnlyMovie ? 'Chapters' : movie.name,
                        style: Theme.of(context).textTheme.labelLarge,
                      ),
                      skeletonHeader: false,
                      query: (q) => client.getChapters(movie.id, q),
                      tileBuilder: (context, item, index) => ThumbnailTile(
                            key: Key('chapter-${item?.id}'),
                            title: item?.name,
                            subtitle: item != null
                                ? formatDuration(item.endTime - item.startTime)
                                : null,
                            thumbnail: item?.thumbnail,
                            onTap: () {
                              context.push(
                                  '/player/movie:${movie.id}?start_pos=${item!.startTime}');
                            },
                          ));
                }).toList()) ??
                [],
            ThumbnailTileGridView(
                key: Key('$packageUuid-extras'),
                header: (movies?.metadata.count ?? 1) > 0
                    ? Text(
                        'Extras',
                        style: Theme.of(context).textTheme.labelLarge,
                      )
                    : null,
                skeletonHeader: movies == null,
                query: (q) =>
                    client.getExtras(packageUuid: packageUuid, page: q),
                tileBuilder: (context, item, index) => ThumbnailTile(
                      key: Key('extra-${item?.id}'),
                      title: item?.name,
                      subtitle:
                          item != null ? formatDuration(item.duration) : null,
                      thumbnail: item?.thumbnail,
                      onTap: () {
                        context.push('/player/extra:${item!.id}');
                      },
                    ))
          ],
        ));
  }
}
