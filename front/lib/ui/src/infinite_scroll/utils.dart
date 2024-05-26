// ignore_for_file: non_constant_identifier_names
import 'package:blee/api/src/models/page.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:infinite_scroll_pagination/infinite_scroll_pagination.dart';
import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';

class InfiniteScrollHelper {
  static Future<void> fetchPage<T>(
      {required Future<page.Page<T>> Function(PageQuery) query,
      required PagingController<int, T> pagingController,
      required int skip,
      int pageSize = 20}) async {
    try {
      final newPage = await query(PageQuery(skip: skip, take: pageSize));
      final isLastPage = newPage.metadata.count < pageSize;
      if (isLastPage) {
        pagingController.appendLastPage(newPage.items);
      } else {
        final nextPageKey = skip + newPage.metadata.count;
        pagingController.appendPage(newPage.items, nextPageKey);
      }
    } catch (error) {
      pagingController.error = error;
    }
  }
}

const padding = 8.0;

SliverGridDelegate DefaultThumbnailTileGridDelegate(BuildContext context) {
  final bpData = ResponsiveBreakpoints.of(context);
  return SliverGridDelegateWithFixedCrossAxisCount(
    childAspectRatio: ThumbnailTile.aspectRatio,
    crossAxisSpacing: padding,
    crossAxisCount: bpData.largerOrEqualTo(BreakpointEnum.xl.name)
        ? 5
        : bpData.largerOrEqualTo(BreakpointEnum.md.name)
            ? 4
            : bpData.largerOrEqualTo(BreakpointEnum.sm.name)
                ? 3
                : 2,
  );
}

SliverGridDelegate DefaultPosterTileGridDelegate(BuildContext context) {
  final bpData = ResponsiveBreakpoints.of(context);
  return SliverGridDelegateWithFixedCrossAxisCount(
    childAspectRatio: PosterTile.aspectRatio,
    crossAxisSpacing: padding,
    mainAxisSpacing: padding,
    crossAxisCount: bpData.largerOrEqualTo(BreakpointEnum.xl.name)
        ? 9
        : bpData.largerOrEqualTo(BreakpointEnum.lg.name)
            ? 8
            : bpData.largerOrEqualTo(BreakpointEnum.md.name)
                ? 6
                : bpData.largerOrEqualTo(BreakpointEnum.sm.name)
                    ? 4
                    : 3,
  );
}
