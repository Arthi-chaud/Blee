import 'package:blee/api/src/client.dart';
import 'package:blee/providers.dart';
import 'package:flutter/material.dart';
import 'package:blee/api/api.dart' as api;
import 'package:flutter_blurhash/flutter_blurhash.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:skeletonizer/skeletonizer.dart';

class Poster extends StatelessWidget {
  final api.Image? image;
  const Poster({super.key, required this.image});

  @override
  Widget build(BuildContext context) {
    const aspectRatio = 3 / 4;
    return _BleeImage(
      key: key,
      image: image,
      placeholderRatio: aspectRatio,
      forcedAspectRatio: aspectRatio,
    );
  }
}

class Thumbnail extends StatelessWidget {
  final api.Image? image;
  final bool disableBorderRadius;
  final bool disableSlashFadein;
  const Thumbnail(
      {super.key,
      required this.image,
      this.disableBorderRadius = false,
      this.disableSlashFadein = false});

  @override
  Widget build(BuildContext context) {
    const aspectRatio = 16 / 9;
    return _BleeImage(
      key: key,
      disableBorderRadius: disableBorderRadius,
      image: image,
      disableSlashFadein: disableSlashFadein,
      placeholderRatio: aspectRatio,
      forcedAspectRatio: aspectRatio,
    );
  }
}

class _BleeImage extends ConsumerWidget {
  final api.Image? image;
  final double? placeholderRatio;
  final double? forcedAspectRatio;
  final bool disableBorderRadius;
  final bool disableSlashFadein;
  const _BleeImage(
      {super.key,
      required this.image,
      this.disableBorderRadius = false,
      this.disableSlashFadein = false,
      this.placeholderRatio,
      this.forcedAspectRatio});

  @override
  Widget build(BuildContext context, ref) {
    APIClient client = ref.read(apiClientProvider);
    return Center(child: Builder(builder: (context) {
      return Skeletonizer(
          enabled: image == null,
          child: Builder(builder: (context) {
            return AspectRatio(
                aspectRatio: forcedAspectRatio ??
                    image?.aspectRatio ??
                    placeholderRatio ??
                    1,
                child: ClipRRect(
                    borderRadius: disableBorderRadius
                        ? BorderRadius.zero
                        : BorderRadius.circular(8),
                    child: Stack(
                      children: [
                        Container(
                            color: disableSlashFadein
                                ? null
                                : Theme.of(context).splashColor),
                        image != null
                            ? BlurHash(
                                image: image == null
                                    ? null
                                    : client.buildImageUrl(image!.id),
                                imageFit: BoxFit.cover,
                                color: disableSlashFadein
                                    ? Colors.transparent
                                    : const Color(0xffebebf4),
                                curve: Curves.easeIn,
                                duration: const Duration(milliseconds: 500),
                                hash: image?.blurhash ??
                                    "L5H2EC=PM+yV0g-mq.wG9c010J}I",
                              )
                            : Container()
                      ],
                    )));
          }));
    }));
  }
}
