import 'package:blee/providers.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/description_box.dart';
import 'package:collection/collection.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:responsive_framework/responsive_framework.dart';

class ArtistPage extends ConsumerWidget {
  final String artistUuid;
  const ArtistPage({super.key, required this.artistUuid});

  @override
  Widget build(BuildContext context, ref) {
    // final artist = ref.watch(getArtistProvider(artistUuid));
    final externalIds = ref.watch(getArtistExternalIdsProvider(artistUuid));
    return MaxWidthBox(
        maxWidth: Breakpoints.getSized(BreakpointEnum.sm),
        child: CustomScrollView(
          slivers: [
            SliverToBoxAdapter(
              child: DescriptionBox(
                description: externalIds.value?.items
                    .map((item) => item.description)
                    .firstWhereOrNull((description) => description != null),
                skeletonize: externalIds.value == null,
              ),
            )
          ],
        ));
  }
}
