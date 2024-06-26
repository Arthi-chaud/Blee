import 'package:blee/ui/src/breakpoints.dart';
import 'package:blee/ui/src/image.dart';
import 'package:flutter/material.dart';
import 'package:responsive_framework/responsive_framework.dart';
import 'package:skeletonizer/skeletonizer.dart';
import 'package:blee/api/src/models/image.dart' as blee_image;

class PosterPageHeader extends StatelessWidget {
  final String title;
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
    final isColumnView =
        ResponsiveBreakpoints.of(context).smallerThan(BreakpointEnum.sm.name);
    var title = Skeletonizer(
        enabled: isLoading,
        child: Text(
          this.title,
          style: Theme.of(context).textTheme.titleLarge,
          textAlign: isColumnView ? TextAlign.center : TextAlign.left,
        ));
    var subtitle =
        Skeletonizer(enabled: isLoading, child: this.subtitle ?? Container());
    var info = thirdTitle;
    paddingForVerticalText(Widget w) => Padding(
          padding: const EdgeInsets.only(left: 14),
          child: w,
        );
    if (isColumnView) {
      return Column(
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.center,
          children: [
            Center(
              child: AspectRatio(
                aspectRatio: 16 / 9,
                child: Poster(image: poster),
              ),
            ),
            Padding(
                padding: const EdgeInsets.only(top: 16, bottom: 4),
                child: title),
            Padding(
              padding: const EdgeInsets.only(top: 4, bottom: 8),
              child: subtitle,
            ),
            info != null
                ? Padding(
                    padding: const EdgeInsets.only(bottom: 8),
                    child: info,
                  )
                : Container()
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
                  paddingForVerticalText(info ?? Container())
                ],
              ),
            ),
          )
        ],
      ),
    );
  }
}
