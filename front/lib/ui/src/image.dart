import 'package:blee/api/src/client.dart';
import 'package:flutter/material.dart';
import 'package:flutter/src/widgets/image.dart' as flutter_image;
import 'package:blee/api/api.dart' as api;
import 'package:flutter_blurhash/flutter_blurhash.dart';

class Poster extends StatelessWidget {
  final api.Image? image;
  const Poster({super.key, required this.image});

  @override
  Widget build(BuildContext context) {
    return _BleeImage(key: key, image: image, placeholderRatio: 3 / 4);
  }
}

class Thumbnail extends StatelessWidget {
  final api.Image? image;
  const Thumbnail({super.key, required this.image});

  @override
  Widget build(BuildContext context) {
    return _BleeImage(key: key, image: image, placeholderRatio: 4 / 3);
  }
}

class _BleeImage extends StatelessWidget {
  final api.Image? image;
  final double? placeholderRatio;
  const _BleeImage({super.key, required this.image, this.placeholderRatio});

  @override
  Widget build(BuildContext context) {
    return Center(child: Builder(builder: (context) {
      if (this.image == null) {
        return AspectRatio(
            aspectRatio: placeholderRatio ?? 1,
            child: ClipRRect(
                borderRadius: BorderRadius.circular(8),
                child: Container(
                  color: Theme.of(context).splashColor,
                )));
      }
      var image = this.image!;
      return AspectRatio(
          aspectRatio: image.aspectRatio,
          child: ClipRRect(
            borderRadius: BorderRadius.circular(8),
            child: flutter_image.Image.network(
              APIClient().buildImageUrl(image.id),
              fit: BoxFit.fill,
              loadingBuilder: (context, child, loadingProgress) {
                if (loadingProgress == null) {
                  return child;
                }
                return BlurHash(
                  hash: image.blurhash,
                );
              },
            ),
          ));
    }));
  }
}
