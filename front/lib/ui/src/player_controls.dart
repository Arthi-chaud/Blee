import 'package:blee/api/src/models/chapter.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/api/api.dart' as api;
import 'package:blee/utils/format_duration.dart';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:video_player/video_player.dart';
import 'package:collection/collection.dart';

class PlayerControls extends StatefulWidget {
  final String? title;
  final String? subtitle;
  final List<Chapter>? chapters;
  final api.Image? poster;
  final int? duration;
  final VideoPlayerController? controller;
  const PlayerControls(
      {super.key,
      required this.title,
      required this.poster,
      required this.chapters,
      required this.subtitle,
      required this.controller,
      required this.duration});

  @override
  State<PlayerControls> createState() => _PlayerControlsState();
}

class _PlayerControlsState extends State<PlayerControls> {
  Duration position = Duration.zero;
  bool isPlaying = true;

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
                              disableSlashFadein: true,
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
                              PlayerSlider(
                                  totalDuration: widget.duration,
                                  currentPosition: widget.controller == null
                                      ? null
                                      : position.inSeconds,
                                  onScroll: (newPosition) => widget.controller
                                      ?.seekTo(Duration(seconds: newPosition)),
                                  chapters: widget.chapters,
                                  buffered: widget.controller?.value.buffered
                                      .lastOrNull?.end.inSeconds),
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

class PlayerSlider extends StatelessWidget {
  final int? totalDuration;
  final int? buffered;
  final int? currentPosition;
  final Function(int) onScroll;
  final List<Chapter>? chapters;
  const PlayerSlider(
      {super.key,
      required this.totalDuration,
      required this.currentPosition,
      required this.onScroll,
      required this.chapters,
      required this.buffered});
  String formatProgress(int? progress) {
    if (progress == null) {
      return '--:--';
    }
    return formatDuration(progress);
  }

  @override
  Widget build(BuildContext context) {
    const textColor = Colors.white60;
    final durationTextStyle = TextStyle(
        color: textColor,
        fontSize: Theme.of(context).textTheme.labelMedium?.fontSize);
    return Row(mainAxisAlignment: MainAxisAlignment.spaceBetween, children: [
      Text(formatProgress(currentPosition), style: durationTextStyle),
      Flexible(
          child: Padding(
              padding: const EdgeInsets.symmetric(horizontal: 8.0),
              child: SliderTheme(
                  data: SliderTheme.of(context).copyWith(
                    trackHeight: 4.0,
                    thumbColor: Colors.transparent,
                    overlayShape: SliderComponentShape.noOverlay,
                    thumbShape:
                        const RoundSliderThumbShape(enabledThumbRadius: 0.0),
                  ),
                  child: Stack(
                    alignment: AlignmentDirectional.center,
                    children: [
                      Slider(
                        min: 0,
                        max: (totalDuration ?? 1).ceilToDouble(),
                        value: currentPosition?.ceilToDouble() ?? 0,
                        secondaryTrackValue: buffered?.ceilToDouble(),
                        onChanged: (scrollPosition) =>
                            onScroll(scrollPosition.ceil()),
                      ),
                      Row(
                        mainAxisSize: MainAxisSize.max,
                        children: chapters?.mapIndexed((chapterIndex, chapter) {
                              final chapterMarkIsBeforeCursor =
                                  chapter.startTime <= (currentPosition ?? 0);
                              return Flexible(
                                  flex: chapter.endTime - chapter.startTime,
                                  child: IgnorePointer(
                                    child: Container(
                                      height: chapterMarkIsBeforeCursor ? 6 : 4,
                                      decoration: BoxDecoration(
                                          border: Border(
                                              left: chapterIndex == 0
                                                  ? BorderSide.none
                                                  : BorderSide(
                                                      color:
                                                          chapterMarkIsBeforeCursor
                                                              ? Colors.white
                                                              : Theme.of(
                                                                      context)
                                                                  .primaryColor,
                                                      width: 2))),
                                    ),
                                  ));
                            }).toList() ??
                            [],
                      )
                    ],
                  )))),
      Text(formatProgress(totalDuration), style: durationTextStyle)
    ]);
  }
}
