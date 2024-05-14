import 'package:blee/ui/src/image.dart';
import 'package:blee/api/api.dart' as api;
import 'package:blee/utils/format_duration.dart';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
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
  bool isPlaying = true;

  String formatProgress(int? progress) {
    if (progress == null) {
      return '--:--';
    }
    return formatDuration(progress);
  }

  void listener() {
    final controller = widget.controller!;
    if (mounted) {
      setState(() {
        isPlaying = controller.value.isPlaying;
        position = controller.value.position;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    const textColor = Colors.white60;
    final durationTextStyle = TextStyle(
        color: textColor,
        fontSize: Theme.of(context).textTheme.labelMedium?.fontSize);

    if (widget.controller != null) {
      widget.controller!.removeListener(listener);
      widget.controller!.addListener(listener);
    }

    return Stack(
      children: [
        // Back Button / Header
        Positioned(
          left: 0,
          top: 0,
          right: 0,
          child: Padding(
            padding: const EdgeInsets.all(4),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              crossAxisAlignment: CrossAxisAlignment.center,
              children: [
                IconButton(
                  icon: const FaIcon(FontAwesomeIcons.chevronLeft, size: 20),
                  color: textColor.withAlpha(150),
                  onPressed: () {
                    // Only useful in debug mode
                    widget.controller?.pause();
                    if (context.canPop()) {
                      context.pop();
                    } else {
                      context.go('/');
                    }
                  },
                ),
                Container(),
              ],
            ),
          ),
        ),
        // Footer
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
                        Colors.black.withAlpha(200),
                        Colors.black
                      ])),
                  child: Padding(
                    padding: const EdgeInsets.all(12),
                    child: Row(
                      children: [
                        // Poster
                        AspectRatio(
                            aspectRatio: 3 / 4,
                            child: Poster(
                              image: widget.poster,
                            )),
                        // Slider + titles
                        Expanded(
                            child: Padding(
                          padding: const EdgeInsets.symmetric(
                              horizontal: 12, vertical: 4),
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.end,
                            crossAxisAlignment: CrossAxisAlignment.start,
                            children: [
                              Column(
                                crossAxisAlignment: CrossAxisAlignment.start,
                                children: [
                                  Text(widget.title ?? '',
                                      maxLines: 1,
                                      overflow: TextOverflow.ellipsis,
                                      style: TextStyle(
                                          color: textColor,
                                          fontSize: Theme.of(context)
                                              .textTheme
                                              .titleMedium
                                              ?.fontSize)),
                                  Padding(
                                    padding:
                                        const EdgeInsets.symmetric(vertical: 4),
                                    child: Row(
                                      mainAxisSize: MainAxisSize.max,
                                      children: [
                                        Expanded(
                                            child: Text(
                                          widget.subtitle ?? '',
                                          maxLines: 1,
                                          overflow: TextOverflow.ellipsis,
                                          style: TextStyle(
                                              color: textColor,
                                              fontSize: Theme.of(context)
                                                  .textTheme
                                                  .labelLarge
                                                  ?.fontSize),
                                        )),
                                        Center(
                                          child: IconButton(
                                            onPressed: () {
                                              if (widget.controller?.value
                                                      .isPlaying ??
                                                  false) {
                                                widget.controller?.pause();
                                              } else {
                                                widget.controller?.play();
                                              }
                                            },
                                            icon: isPlaying
                                                ? const FaIcon(
                                                    FontAwesomeIcons.pause,
                                                    size: 20)
                                                : const FaIcon(
                                                    FontAwesomeIcons.play,
                                                    size: 15),
                                            color: Colors.white,
                                          ),
                                        ),
                                        Expanded(child: Container())
                                      ],
                                    ),
                                  ),
                                ],
                              ),
                              Row(
                                mainAxisAlignment:
                                    MainAxisAlignment.spaceBetween,
                                children: [
                                  Text(
                                      formatProgress(widget.controller == null
                                          ? null
                                          : position.inSeconds),
                                      style: durationTextStyle),
                                  Flexible(
                                      child: Padding(
                                    padding: const EdgeInsets.symmetric(
                                        horizontal: 8.0),
                                    child: SliderTheme(
                                      data: SliderTheme.of(context).copyWith(
                                        trackHeight: 4.0,
                                        thumbColor: Colors.transparent,
                                        overlayShape:
                                            SliderComponentShape.noThumb,
                                        thumbShape: const RoundSliderThumbShape(
                                            enabledThumbRadius: 0.0),
                                      ),
                                      child: Slider(
                                          min: 0,
                                          max: (widget.duration ?? 1)
                                              .ceilToDouble(),
                                          secondaryTrackValue: widget
                                              .controller
                                              ?.value
                                              .buffered
                                              .lastOrNull
                                              ?.end
                                              .inSeconds
                                              .ceilToDouble(),
                                          value:
                                              position.inSeconds.ceilToDouble(),
                                          onChanged: (scrollPosition) {
                                            widget.controller?.seekTo(Duration(
                                                seconds:
                                                    scrollPosition.ceil()));
                                          }),
                                    ),
                                  )),
                                  Text(formatProgress(widget.duration),
                                      style: durationTextStyle)
                                ],
                              ),
                            ],
                          ),
                        ))
                      ],
                    ),
                  ),
                )))
      ],
    );
  }
}
