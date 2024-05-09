import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/description_box.dart';
import 'package:blee/ui/src/infinite_scroll/infinite_grid.dart';
import 'package:blee/ui/src/poster_page_header.dart';
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
                PosterPageHeader(
                    key: Key('$packageUuid-header'),
                    isLoading: package == null,
                    title: Text(
                      package?.name ?? 'No Package Name',
                      style: Theme.of(context).textTheme.titleLarge,
                    ),
                    subtitle: TextButton(
                        onPressed: () =>
                            context.push('/artists/${package?.artistId}'),
                        child: Text(
                          package?.artistName ?? 'No Artist Name',
                          style: Theme.of(context).textTheme.titleMedium,
                        )),
                    thirdTitle: Text(
                      package?.releaseDate?.year.toString() ?? '',
                      style: Theme.of(context).textTheme.labelLarge,
                    ),
                    poster: package?.poster),
                SizedBox.fromSize(size: const Size.fromHeight(8)),
                DescriptionBox(
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
                  return ThumbnailGridView(
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
            ThumbnailGridView(
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
