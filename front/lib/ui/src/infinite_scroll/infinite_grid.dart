import 'package:blee/api/api.dart';
import 'package:blee/api/src/models/page.dart' as page;
import 'package:blee/ui/src/infinite_scroll/utils.dart';
import 'package:flutter/material.dart';
import 'package:flutter_sticky_header/flutter_sticky_header.dart';
import 'package:infinite_scroll_pagination/infinite_scroll_pagination.dart';
import 'package:skeletonizer/skeletonizer.dart';

class ThumbnailGridView<T> extends AbstractGridView<T> {
  const ThumbnailGridView(
      {super.key,
      required super.tileBuilder,
      required super.query,
      super.skeletonHeader,
      required super.header})
      : super(delegate: DefaultThumbnailTileGridDelegate);
}

class PosterGridView<T> extends AbstractGridView<T> {
  const PosterGridView(
      {super.key,
      required super.tileBuilder,
      required super.query,
      super.skeletonHeader,
      required super.header})
      : super(delegate: DefaultPosterTileGridDelegate);
}

abstract class AbstractGridView<T> extends StatefulWidget {
  final Future<page.Page<T>> Function(PageQuery) query;
  final Widget Function(BuildContext, T?, int) tileBuilder;
  final SliverGridDelegate Function(BuildContext) delegate;
  final Widget? header;
  final bool? skeletonHeader;
  const AbstractGridView(
      {super.key,
      required this.query,
      required this.tileBuilder,
      required this.header,
      required this.delegate,
      this.skeletonHeader});
  @override
  State<AbstractGridView<T>> createState() => _AbstractGridViewState<T>();
}

class _AbstractGridViewState<T> extends State<AbstractGridView<T>> {
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

  @override
  Widget build(BuildContext context) => SliverStickyHeader(
      sticky: widget.header != null,
      header: Container(
        color: Theme.of(context).scaffoldBackgroundColor,
        child: Skeletonizer(
          enabled: widget.skeletonHeader ?? _pagingController.itemList == null,
          child: Padding(
              padding: const EdgeInsets.symmetric(vertical: 8, horizontal: 4),
              child: (_pagingController.itemList?.isEmpty ?? false)
                  ? Container()
                  : widget.header ?? Container()),
        ),
      ),
      sliver: PagedSliverGrid<int, T>(
        addAutomaticKeepAlives: false,
        pagingController: _pagingController,
        gridDelegate: widget.delegate(context),
        shrinkWrapFirstPageIndicators: true,
        builderDelegate: PagedChildBuilderDelegate<T>(
          noItemsFoundIndicatorBuilder: (_) => Container(),
          newPageProgressIndicatorBuilder: (_) => Container(),
          firstPageProgressIndicatorBuilder: (context) => GridView(
            shrinkWrap: true,
            physics: const NeverScrollableScrollPhysics(),
            gridDelegate: widget.delegate(context),
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
