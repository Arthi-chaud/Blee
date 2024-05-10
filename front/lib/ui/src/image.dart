import 'package:blee/api/src/client.dart';
import 'package:blee/providers.dart';
import 'package:flutter/material.dart';
import 'package:blee/api/api.dart' as api;
import 'package:flutter_blurhash/flutter_blurhash.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:skeletonizer/skeletonizer.dart';

class Poster extends _BleeImage {
  const Poster(
      {super.key,
      required super.image,
      super.disableBorderRadius = false,
      super.onTap,
      super.disableSlashFadein = false})
      : super(placeholderAspectRatio: 3 / 4, fitToPlaceholder: false);
}

class Thumbnail extends _BleeImage {
  const Thumbnail(
      {super.key,
      required super.image,
      super.onTap,
      super.disableBorderRadius = false,
      super.disableSlashFadein = false})
      : super(placeholderAspectRatio: 16 / 9, fitToPlaceholder: true);
}

abstract class _BleeImage extends ConsumerWidget {
  final api.Image? image;
  final double placeholderAspectRatio;
  final bool fitToPlaceholder;
  final bool disableBorderRadius;
  final bool disableSlashFadein;
  final Function? onTap;
  const _BleeImage(
      {super.key,
      required this.image,
      this.disableBorderRadius = false,
      this.disableSlashFadein = false,
      this.fitToPlaceholder = true,
      this.onTap,
      required this.placeholderAspectRatio});

  Widget roundedContainer(Widget child) {
    return ClipRRect(
        borderRadius:
            disableBorderRadius ? BorderRadius.zero : BorderRadius.circular(8),
        child: AspectRatio(
            aspectRatio: fitToPlaceholder
                ? placeholderAspectRatio
                : image?.aspectRatio ?? placeholderAspectRatio,
            child: child));
  }

  @override
  Widget build(BuildContext context, ref) {
    APIClient client = ref.read(apiClientProvider);
    return Center(child: Builder(builder: (context) {
      return Skeletonizer(
          enabled: false,
          child: AspectRatio(
              aspectRatio: placeholderAspectRatio,
              child: Stack(
                alignment: AlignmentDirectional.center,
                children: [
                  roundedContainer(Container(
                      color: disableSlashFadein
                          ? null
                          : Theme.of(context).splashColor)),
                  AspectRatio(
                      aspectRatio: fitToPlaceholder
                          ? placeholderAspectRatio
                          : image?.aspectRatio ?? placeholderAspectRatio,
                      child: Stack(children: [
                        image != null
                            ? roundedContainer(BlurHash(
                                image: image == null
                                    ? null
                                    : client.buildImageUrl(image!.id),
                                imageFit: BoxFit.cover,
                                color: disableSlashFadein
                                    ? Colors.transparent
                                    : const Color(0xffebebf4),
                                curve: Curves.easeIn,
                                duration: const Duration(milliseconds: 200),
                                hash: image?.blurhash ??
                                    "L5H2EC=PM+yV0g-mq.wG9c010J}I",
                              ))
                            : Container(),
                        onTap != null
                            ? Positioned.fill(
                                child: ClipRRect(
                                    borderRadius: BorderRadius.circular(8),
                                    child: Material(
                                      color: Colors.transparent,
                                      child: InkWell(
                                        onTap: () {
                                          onTap!.call();
                                        },
                                      ),
                                    )),
                              )
                            : Container(),
                      ]))
                ],
              )));
    }));
  }
}
