import 'package:blee/api/src/models/page.dart';
import 'package:blee/ui/src/infinite_scroll/utils.dart';
import 'package:blee/ui/src/tile.dart';
import 'package:flutter/material.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:infinite_scroll_pagination/infinite_scroll_pagination.dart';

class PosterTileListView<T> extends AbstractHorizontalListView<T> {
  final Widget Function(BuildContext, T?, int) itemBuilder;
  PosterTileListView(
      {super.key,
      required this.itemBuilder,
      required super.query,
      super.skeletonHeader,
      required super.header})
      : super(
            tileBuilder: (context, item, index) => AspectRatio(
                aspectRatio: PosterTile.aspectRatio,
                child: itemBuilder(context, item, index)));
}

abstract class AbstractHorizontalListView<T> extends StatefulWidget {
  final Future<page.Page<T>> Function(PageQuery) query;
  final Widget Function(BuildContext, T?, int) tileBuilder;
  final Widget? header;
  final bool? skeletonHeader;
  const AbstractHorizontalListView(
      {super.key,
      required this.query,
      required this.tileBuilder,
      required this.header,
      this.skeletonHeader});
  @override
  State<AbstractHorizontalListView<T>> createState() =>
      _AbstractHorizontalListViewState<T>();
}

class _AbstractHorizontalListViewState<T>
    extends State<AbstractHorizontalListView<T>> {
  final PagingController<int, T> _pagingController =
      PagingController(firstPageKey: 0);

  @override
  void initState() {
    _pagingController.addPageRequestListener((pageKey) {
      InfiniteScrollHelper.fetchPage(
          query: widget.query,
          skip: pageKey,
          pagingController: _pagingController);
    });
    super.initState();
  }

  Widget _itemBuilder(Widget item) =>
      Padding(padding: const EdgeInsets.all(4), child: item);

  @override
  Widget build(BuildContext context) {
    return PagedListView<int, T>(
      primary: false,
      pagingController: _pagingController,
      scrollDirection: Axis.horizontal,
      builderDelegate: PagedChildBuilderDelegate<T>(
        noItemsFoundIndicatorBuilder: (_) => Container(),
        firstPageProgressIndicatorBuilder: (context) => Row(
          children: [0, 1]
              .map((index) =>
                  _itemBuilder(widget.tileBuilder(context, null, index)))
              .toList(),
        ),
        itemBuilder: (context, item, index) =>
            _itemBuilder(widget.tileBuilder(context, item, index)),
      ),
    );
  }

  @override
  void dispose() {
    _pagingController.dispose();
    super.dispose();
  }
}
