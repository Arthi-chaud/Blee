import 'package:blee/ui/src/image.dart';
import 'package:blee/api/api.dart' as api;
import 'package:blee/utils/format_duration.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:video_player/video_player.dart';

class PlayerControls extends StatefulWidget {
  final String? title;
  final String? subtitle;
  final api.Image? poster;
  final int? duration;
  final VideoPlayerController? controller;
  const PlayerControls(
      {super.key,
      required this.title,
      required this.poster,
      required this.subtitle,
      required this.controller,
      required this.duration});

  @override
  State<PlayerControls> createState() => _PlayerControlsState();
}

class _PlayerControlsState extends State<PlayerControls> {
  Duration position = Duration.zero;
  String formatProgress(int? progress) {
    if (progress == null) {
      return '--:--';
    }
    return formatDuration(progress);
  }

  void listener() {
    if (mounted) {
      setState(() {
        position = widget.controller!.value.position;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    final durationTextStyle = TextStyle(color: Theme.of(context).dividerColor);

    if (widget.controller != null) {
      widget.controller!.removeListener(listener);
      widget.controller!.addListener(listener);
    }

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
                  context.push('/');
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
                height: 140,
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
                              image: widget.poster,
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
                                  Text(
                                      formatProgress(widget.controller == null
                                          ? null
                                          : position.inSeconds),
                                      style: durationTextStyle),
                                  Text(widget.title ?? '',
                                      style: durationTextStyle),
                                  Text(formatProgress(widget.duration),
                                      style: durationTextStyle)
                                ],
                              ),
                            ),
                            Slider(
                                min: 0,
                                max: (widget.duration ?? 1).ceilToDouble(),
                                value: position.inSeconds.ceilToDouble(),
                                onChanged: (scrollPosition) {
                                  widget.controller?.seekTo(
                                      Duration(seconds: scrollPosition.ceil()));
                                }),
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
