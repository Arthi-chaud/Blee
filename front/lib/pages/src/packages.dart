import 'package:blee/api/api.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/infinite_scroll/infinite_grid.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class PackagesPage extends ConsumerWidget {
  const PackagesPage({super.key});
  @override
  Widget build(BuildContext context, ref) {
    APIClient client = ref.read(apiClientProvider);

    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.lg),
        child: CustomScrollView(slivers: [
          PosterTileGridView<Package>(
              tileBuilder: (context, item, index) => PosterTile(
                    onTap: () => context.push('/packages/${item?.id}'),
                    title: item?.name,
                    subtitle: item?.artistName,
                    thumbnail: item?.poster,
                  ),
              query: (q) => client.getPackages(page: q),
              header: null)
        ]));
  }
}
