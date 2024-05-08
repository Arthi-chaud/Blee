import 'package:blee/ui/src/image.dart';
import 'package:blee/api/api.dart' as api;
import 'package:blee/utils/format_duration.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

class PlayerControls extends StatelessWidget {
  final String? title;
  final String? subtitle;
  final api.Image? poster;
  final int? progress;
  final int? duration;
  const PlayerControls(
      {super.key,
      required this.title,
      required this.poster,
      required this.subtitle,
      required this.progress,
      required this.duration});

  String formatProgress(int? progress) {
    if (progress == null) {
      return '--:--';
    }
    return formatDuration(progress);
  }

  @override
  Widget build(BuildContext context) {
    final durationTextStyle = TextStyle(color: Theme.of(context).dividerColor);
    return Stack(
      children: [
        Positioned(
          left: 0,
          top: 0,
          child: Padding(
            padding: const EdgeInsets.all(4),
            child: IconButton(
              icon: const Icon(Icons.arrow_back, size: 25),
              onPressed: () {
                if (context.canPop()) {
                  context.pop();
                } else {
                  context.push('/packages');
                }
              },
            ),
          ),
        ),
        Positioned(
            bottom: 0,
            left: 0,
            right: 0,
            child: SizedBox(
                height: 180,
                child: Container(
                  decoration: BoxDecoration(
                      gradient: LinearGradient(
                          begin: Alignment.topCenter,
                          end: Alignment.bottomCenter,
                          colors: [
                        Colors.transparent,
                        Colors.white.withAlpha(50)
                      ])),
                  child: Padding(
                    padding: const EdgeInsets.all(12),
                    child: Row(
                      children: [
                        AspectRatio(
                            aspectRatio: 3 / 4,
                            child: Poster(
                              image: poster,
                            )),
                        Expanded(
                            child: Column(
                          mainAxisAlignment: MainAxisAlignment.end,
                          children: [
                            Padding(
                              padding:
                                  const EdgeInsets.symmetric(horizontal: 12),
                              child: Row(
                                mainAxisAlignment:
                                    MainAxisAlignment.spaceBetween,
                                children: [
                                  Text(formatProgress(progress),
                                      style: durationTextStyle),
                                  Text(title ?? '', style: durationTextStyle),
                                  Text(formatProgress(duration),
                                      style: durationTextStyle)
                                ],
                              ),
                            ),
                            Slider(
                                value: progress?.ceilToDouble() ?? 0,
                                onChanged: (_) {}),
                          ],
                        ))
                      ],
                    ),
                  ),
                )))
      ],
    );
  }
}
