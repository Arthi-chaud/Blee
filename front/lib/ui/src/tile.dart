import 'package:blee/api/api.dart' as api;
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';
import 'package:skeletonizer/skeletonizer.dart';

class PosterTile extends Tile {
  static double aspectRatio = 0.55;
  PosterTile(
      {super.key,
      required super.title,
      required super.subtitle,
      required super.thumbnail,
      required super.onTap})
      : super(
            imageWidgetBuilder: (image, onTap) =>
                Poster(image: image, onTap: onTap));
}

class ThumbnailTile extends Tile {
  static double aspectRatio = 1.22;
  ThumbnailTile(
      {super.key,
      required super.title,
      required super.subtitle,
      required super.thumbnail,
      required super.onTap})
      : super(
            imageWidgetBuilder: (image, onTap) =>
                Thumbnail(image: image, onTap: onTap));
}

abstract class Tile extends StatelessWidget {
  final String? title;
  final String? subtitle;
  final api.Image? thumbnail;
  final Function? onTap;
  final Widget Function(api.Image?, Function? onTap) imageWidgetBuilder;
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
        imageWidgetBuilder(thumbnail, onTap),
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
