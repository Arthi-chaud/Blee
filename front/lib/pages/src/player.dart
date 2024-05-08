import 'dart:convert';

import 'package:blee/models/models.dart';
import 'package:blee/api/api.dart' as api;
import 'package:blee/providers.dart';
import 'package:blee/ui/src/hide_on_inactivity.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/player_controls.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:video_player/video_player.dart';
import 'package:collection/collection.dart';

class PlayerPage extends ConsumerStatefulWidget {
  final String? extraUuid;
  final String? movieUuid;
  final int? startPosition;
  const PlayerPage(
      {super.key, this.movieUuid, this.extraUuid, this.startPosition});

  @override
  PlayerPageState createState() => PlayerPageState();
}

enum StreamMode { direct, hls }

class PlayerPageState extends ConsumerState<PlayerPage> {
  PlayerFlowStep flowStep = PlayerFlowStep.loadingNeededMetadata;
  late AutoDisposeFutureProvider<PlayerMetadata> videoMetadataProvider;
  VideoPlayerController? _controller;
  api.Chapter? currentChapter;

  @override
  void initState() {
    super.initState();
    videoMetadataProvider = widget.extraUuid != null
        ? getPlayerMetadataFromExtraUuidProvider(widget.extraUuid!)
        : getPlayerMetadataFromMovieUuidProvider(widget.movieUuid!);
  }

  @override
  void dispose() {
    super.dispose();
    _controller?.pause();
    _controller?.dispose();
  }

  void _onPlayerInit(PlayerMetadata metadata) {
    _controller?.setVolume(1);
    if (widget.startPosition == null) {
      _controller!.play();
    } else {
      _controller!
          .seekTo(Duration(seconds: widget.startPosition!))
          .then((_) => _controller!.play());
    }
    _controller!.addListener(() {
      if (metadata.chapters.isNotEmpty) {
        // If we are in a movie
        var currentPlayerPosition = _controller!.value.position.inSeconds;
        // If the chapter was not previously set
        if (currentChapter == null ||
            currentPlayerPosition < currentChapter!.startTime ||
            currentChapter!.endTime < currentPlayerPosition) {
          setState(() {
            currentChapter = metadata.chapters.firstWhereOrNull((chapter) =>
                chapter.startTime <= currentPlayerPosition &&
                currentPlayerPosition < chapter.endTime);
          });
        }
      }
      // BUG in video_player: `isCompleted` is fired twice
      // need to check the actual position of the player to pop exactly one
      if (_controller!.value.isCompleted &&
          _controller!.value.position.inSeconds ==
              metadata.videoFile.duration) {
        if (!mounted) return;
        if (context.canPop()) {
          context.pop();
        } else {
          context.go('/');
        }
      }
    });
    setState(() {
      flowStep = PlayerFlowStep.playerStarted;
    });
  }

  VideoPlayerController setupPlayer(PlayerMetadata metadata,
      {StreamMode streamMode = StreamMode.direct}) {
    final baseUrl =
        'http://localhost:7666/${base64Encode(utf8.encode(metadata.videoFile.path))}';
    return VideoPlayerController.networkUrl(
        Uri.parse(streamMode == StreamMode.direct
            ? '$baseUrl/direct'
            : '$baseUrl/original/index.m3u8'),
        httpHeaders: {"X-CLIENT-ID": "A"})
      ..initialize().then((_) => _onPlayerInit(metadata));
  }

  @override
  Widget build(BuildContext context) {
    final metadata = ref.watch(videoMetadataProvider)
      ..when(
          data: (m) {
            // TODO
            // How to listen to videoMetadataProvider
            // so that we can setup the controller outside the build method
            if (_controller == null) {
              setState(() {
                flowStep = PlayerFlowStep.loadingPlayer;
                _controller = setupPlayer(m);
              });
            }
          },
          error: (_, __) {
            setState(() {
              flowStep = PlayerFlowStep.errored;
            });
          },
          loading: () {});
    return Scaffold(
        backgroundColor: Colors.black,
        body: Stack(
          children: [
            AnimatedOpacity(
                duration: const Duration(milliseconds: 2000),
                opacity: flowStep == PlayerFlowStep.playerStarted ? 0 : 1,
                child: Stack(
                  children: [
                    // TODO: Thumbnail should fill screen
                    Thumbnail(
                        image: metadata.value?.thumbnail,
                        disableSlashFadein: true,
                        disableBorderRadius: true),
                    Container(color: Colors.black.withAlpha(200)),
                    const Center(
                      child: CircularProgressIndicator(),
                    ),
                  ],
                )),
            AnimatedOpacity(
              duration: const Duration(milliseconds: 500),
              opacity: flowStep != PlayerFlowStep.playerStarted ? 0 : 1,
              child: _controller?.value.isInitialized ?? false
                  ? Center(
                      child: AspectRatio(
                      aspectRatio: _controller!.value.aspectRatio,
                      child: VideoPlayer(_controller!),
                    ))
                  : Container(),
            ),
            HideOnInactivity(
                child: PlayerControls(
                  title: metadata.value?.videoTitle,
                  poster: metadata.value?.poster,
                  subtitle: currentChapter != null
                      ? '${currentChapter!.name} - ${metadata.value?.videoArtist ?? ''}'
                      : metadata.value?.videoArtist,
                  duration: metadata.value?.videoFile.duration,
                  controller: _controller,
                )),
          ],
        ));
  }
}
