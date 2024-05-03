import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/ui/src/grid.dart';
import 'package:flutter/material.dart';
import 'package:flutter_sticky_header/flutter_sticky_header.dart';
import 'package:infinite_scroll_pagination/infinite_scroll_pagination.dart';
import 'package:skeletonizer/skeletonizer.dart';

class ThumbnailGridView<T> extends StatefulWidget {
  final Future<page.Page<T>> Function(PageQuery) query;
  final Widget Function(BuildContext, T?, int) tileBuilder;
  final Widget? header;
  final bool? skeletonHeader;
  const ThumbnailGridView(
      {super.key,
      required this.query,
      required this.tileBuilder,
      required this.header,
      this.skeletonHeader});
  @override
  State<ThumbnailGridView<T>> createState() => _ThumbnailGridViewState<T>();
}

class _ThumbnailGridViewState<T> extends State<ThumbnailGridView<T>> {
  static const _pageSize = 20;

  final PagingController<int, T> _pagingController =
      PagingController(firstPageKey: 0);

  @override
  void initState() {
    _pagingController.addPageRequestListener((pageKey) {
      _fetchPage(pageKey);
    });
    super.initState();
  }

  Future<void> _fetchPage(int pageKey) async {
    try {
      final newPage =
          await widget.query(PageQuery(skip: pageKey, take: _pageSize));
      final isLastPage = newPage.metadata.count < _pageSize;
      if (isLastPage) {
        _pagingController.appendLastPage(newPage.items);
      } else {
        final nextPageKey = pageKey + newPage.metadata.count;
        _pagingController.appendPage(newPage.items, nextPageKey);
      }
    } catch (error) {
      _pagingController.error = error;
    }
  }

  @override
  Widget build(BuildContext context) => SliverStickyHeader(
      header: Padding(
          padding: const EdgeInsets.symmetric(vertical: 8, horizontal: 4),
          child: Skeletonizer(
            enabled:
                widget.skeletonHeader ?? _pagingController.itemList == null,
            child: (_pagingController.itemList?.isEmpty ?? false)
                ? Container()
                : widget.header ?? Container(),
          )),
      sliver: PagedSliverGrid<int, T>(
        pagingController: _pagingController,
        gridDelegate: DefaultThumbnailTileGridDelegate(context),
        shrinkWrapFirstPageIndicators: true,
        builderDelegate: PagedChildBuilderDelegate<T>(
          noItemsFoundIndicatorBuilder: (_) => Container(),
          firstPageProgressIndicatorBuilder: (context) => GridView(
            shrinkWrap: true,
            physics: const NeverScrollableScrollPhysics(),
            gridDelegate: DefaultThumbnailTileGridDelegate(context),
            children: [0, 1]
                .map((index) => widget.tileBuilder(context, null, index))
                .toList(),
          ),
          itemBuilder: (context, item, index) =>
              widget.tileBuilder(context, item, index),
        ),
      ));

  @override
  void dispose() {
    _pagingController.dispose();
    super.dispose();
  }
}
