import 'package:blee/api/api.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/infinite_scroll/infinite_grid.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class ArtistsPage extends ConsumerWidget {
  const ArtistsPage({super.key});
  @override
  Widget build(BuildContext context, ref) {
    APIClient client = ref.read(apiClientProvider);

    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.lg),
        child: CustomScrollView(slivers: [
          PosterTileGridView<Artist>(
              tileBuilder: (context, item, index) => PosterTile(
                    onTap: () => context.push('/artists/${item?.id}'),
                    title: item?.name,
                    subtitle: '',
                    thumbnail: item?.poster,
                  ),
              query: (q) => client.getArtists(page: q),
              header: null)
        ]));
  }
}
