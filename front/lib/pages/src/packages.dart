import 'package:blee/api/api.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/infinite_scroll.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class PackagesPage extends StatelessWidget {
  const PackagesPage({super.key});
  @override
  Widget build(BuildContext context) {
    final client = APIClient();

    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.lg),
        child: CustomScrollView(slivers: [
          PosterTileGridView<Package>(
              tileBuilder: (context, item, index) => PosterTile(
                    onTap: () => context.go('/packages/${item?.id}'),
                    title: item?.name,
                    subtitle: item?.artistName,
                    thumbnail: item?.poster,
                  ),
              query: (q) => client.getPackages(page: q),
              header: Container())
        ]));
  }
}
