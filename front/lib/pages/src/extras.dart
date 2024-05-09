import 'package:blee/api/api.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/infinite_scroll.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class ExtrasPage extends ConsumerWidget {
  const ExtrasPage({super.key});
  @override
  Widget build(BuildContext context, ref) {
    final client = ref.read(apiClientProvider);

    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
        child: CustomScrollView(slivers: [
          ThumbnailTileGridView<Extra>(
              tileBuilder: (context, item, index) => ThumbnailTile(
                    onTap: () => context.push('/player/extra:${item!.id}'),
                    title: item?.name,
                    subtitle: item?.artistName,
                    thumbnail: item?.thumbnail,
                  ),
              query: (q) => client.getExtras(page: q),
              header: null)
        ]));
  }
}
