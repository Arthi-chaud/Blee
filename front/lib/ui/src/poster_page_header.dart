import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';
import 'package:skeletonizer/skeletonizer.dart';
import 'package:blee/api/src/models/image.dart' as blee_image;

class PosterPageHeader extends StatelessWidget {
  final Widget title;
  final Widget? subtitle;
  final Widget? thirdTitle;
  final blee_image.Image? poster;
  final bool isLoading;
  const PosterPageHeader(
      {super.key,
      required this.title,
      required this.subtitle,
      required this.thirdTitle,
      required this.isLoading,
      required this.poster});

  @override
  Widget build(BuildContext context) {
    var title = Skeletonizer(enabled: isLoading, child: this.title);
    var subtitle =
        Skeletonizer(enabled: isLoading, child: this.subtitle ?? Container());
    var info = thirdTitle ?? Container();
    paddingForVerticalText(Widget w) => Padding(
          padding: const EdgeInsets.only(left: 14),
          child: w,
        );
    if (ResponsiveBreakpoints.of(context).smallerThan(BreakpointEnum.sm.name)) {
      return Column(mainAxisSize: MainAxisSize.min, children: [
        Center(
          child: AspectRatio(
            aspectRatio: 16 / 9,
            child: Poster(image: poster),
          ),
        ),
        Padding(
          padding: const EdgeInsets.only(top: 16, bottom: 4),
          child: title,
        ),
        subtitle,
        info
      ]);
    }
    return IntrinsicHeight(
      child: Row(
        children: [
          Flexible(child: Poster(image: poster)),
          Flexible(
            flex: 2,
            child: Padding(
              padding: const EdgeInsets.only(left: 8),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
                  paddingForVerticalText(title),
                  subtitle,
                  paddingForVerticalText(info)
                ],
              ),
            ),
          )
        ],
      ),
    );
  }
}
