import 'package:blee/api/api.dart' as api;
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';
import 'package:skeletonizer/skeletonizer.dart';

class PosterTile extends Tile {
  PosterTile(
      {super.key,
      required super.title,
      required super.subtitle,
      required super.thumbnail,
      required super.onTap})
      : super(imageWidgetBuilder: (image) => Poster(image: image));
}

class ThumbnailTile extends Tile {
  ThumbnailTile(
      {super.key,
      required super.title,
      required super.subtitle,
      required super.thumbnail,
      required super.onTap})
      : super(imageWidgetBuilder: (image) => Thumbnail(image: image));
}

abstract class Tile extends StatelessWidget {
  final String? title;
  final String? subtitle;
  final api.Image? thumbnail;
  final Function? onTap;
  final Widget Function(api.Image?) imageWidgetBuilder;
  const Tile(
      {super.key,
      required this.title,
      required this.subtitle,
      required this.thumbnail,
      required this.imageWidgetBuilder,
      required this.onTap});

  @override
  Widget build(BuildContext context) {
    var isLoading = title == null;
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Stack(
          children: [
            imageWidgetBuilder(thumbnail),
            Positioned.fill(
              child: ClipRRect(
                  borderRadius: BorderRadius.circular(8),
                  child: Material(
                    color: Colors.transparent,
                    child: isLoading
                        ? Container()
                        : InkWell(
                            onTap: () {
                              onTap?.call();
                            },
                          ),
                  )),
            ),
          ],
        ),
        Padding(
          padding: const EdgeInsets.only(top: 4, left: 4),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Skeletonizer(
                enabled: isLoading,
                child: Text(
                  title ?? 'No Name',
                  maxLines: 1,
                  overflow: TextOverflow.ellipsis,
                  style: Theme.of(context).textTheme.labelLarge,
                ),
              ),
              Skeletonizer(
                  enabled: isLoading,
                  child: Text(
                    subtitle ?? '0:00',
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                    style: Theme.of(context).textTheme.labelSmall,
                  ))
            ],
          ),
        )
      ],
    );
  }
}
