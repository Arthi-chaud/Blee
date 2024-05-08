import 'dart:convert';

import 'package:blee/models/models.dart';
import 'package:blee/providers.dart';
import 'package:blee/ui/src/image.dart';
import 'package:blee/ui/src/player_controls.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:video_player/video_player.dart';

class PlayerPage extends ConsumerStatefulWidget {
  final String? extraUuid;
  final String? movieUuid;
  const PlayerPage({super.key, this.movieUuid, this.extraUuid});

  @override
  PlayerPageState createState() => PlayerPageState();
}

class PlayerPageState extends ConsumerState<PlayerPage> {
  PlayerFlowStep flowStep = PlayerFlowStep.loadingNeededMetadata;
  late AutoDisposeFutureProvider<PlayerMetadata> videoMetadataProvider;
  VideoPlayerController? _controller;

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

  VideoPlayerController setupPlayer(PlayerMetadata metadata) {
    return VideoPlayerController.networkUrl(Uri.parse(
        'http://localhost:7666/${base64Encode(utf8.encode(metadata.videoFile.path))}/direct'))
      ..initialize().then((_) {
        _controller!.play();
        _controller!.addListener(() {
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
      });
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
            PlayerControls(
              title: metadata.value?.videoTitle,
              poster: metadata.value?.poster,
              subtitle: metadata.value?.videoArtist,
              duration: metadata.value?.videoFile.duration,
              controller: _controller,
            ),
          ],
        ));
  }
}
