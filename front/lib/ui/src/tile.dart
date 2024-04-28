import 'package:blee/api/api.dart' as api;
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';

class Tile extends StatelessWidget {
  final String? title;
  final String? subtitle;
  final api.Image? thumbnail;
  const Tile(
      {super.key,
      required this.title,
      required this.subtitle,
      required this.thumbnail});

  @override
  Widget build(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        AspectRatio(
          aspectRatio: 4 / 3,
          child: Thumbnail(image: thumbnail),
        ),
        Padding(
          padding: const EdgeInsets.only(top: 4, left: 4),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                title ?? '',
                maxLines: 1,
                overflow: TextOverflow.ellipsis,
                style: Theme.of(context).textTheme.labelLarge,
              ),
              Text(
                subtitle ?? '',
                maxLines: 1,
                overflow: TextOverflow.ellipsis,
                style: Theme.of(context).textTheme.labelSmall,
              )
            ],
          ),
        )
      ],
    );
  }
}
