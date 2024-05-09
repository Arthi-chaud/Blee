import 'package:blee/api/api.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/description_box.dart';
import 'package:blee/ui/src/infinite_scroll.dart';
import 'package:blee/ui/src/poster_page_header.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:collection/collection.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class ArtistPage extends ConsumerWidget {
  final String artistUuid;
  const ArtistPage({super.key, required this.artistUuid});

  @override
  Widget build(BuildContext context, ref) {
    final client = ref.watch(apiClientProvider);
    final artist = ref.watch(getArtistProvider(artistUuid));
    final externalIds = ref.watch(getArtistExternalIdsProvider(artistUuid));
    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
        child: ListView(children: [
          SizedBox.fromSize(size: const Size.fromHeight(16)),
          PosterPageHeader(
            isLoading: artist.value == null,
            title: Text(
              artist.value?.name ?? 'No Artist Name',
              style: Theme.of(context).textTheme.titleLarge,
            ),
            subtitle: null,
            thirdTitle: null,
            poster: artist.value?.poster,
          ),
          Padding(
            padding: const EdgeInsets.symmetric(vertical: 8.0),
            child: DescriptionBox(
              description: externalIds.value?.items
                  .map((item) => item.description)
                  .firstWhereOrNull((description) =>
                      description != null && description.length > 1),
              skeletonize: externalIds.value == null,
            ),
          ),
          artist.value == null
              ? Container()
              : SizedBox(
                  height: 200,
                  child: PosterTileListView<Package>(
                      tileBuilder: (context, item, index) => PosterTile(
                            onTap: () => context.push('/packages/${item?.id}'),
                            title: item?.name,
                            subtitle: item?.releaseDate?.year.toString() ?? '',
                            thumbnail: item?.poster,
                          ),
                      query: (q) => client.getPackages(
                          page: q, artistUuid: artist.value?.id),
                      header: Text('Packages')))
        ]));
  }
}
