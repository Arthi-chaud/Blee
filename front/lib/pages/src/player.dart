import 'dart:convert';
import 'package:blee/api/src/client.dart';
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
import 'package:mediametadata/mediametadata.dart';
import 'package:uuid/uuid.dart';

class PlayerPage extends ConsumerStatefulWidget {
  final clientId = const Uuid().v4();
  final String? extraUuid;
  final String? movieUuid;
  final int? startPosition;
  PlayerPage({super.key, this.movieUuid, this.extraUuid, this.startPosition});

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

  void _onPlayerInit(PlayerMetadata metadata, int? startPos) {
    _controller!.play().then((_) {
      if (widget.startPosition != null) {
        _controller!.seekTo(Duration(seconds: widget.startPosition!));
      }
    });
    _controller!.addListener(() {
      // If we are in a movie
      if (metadata.chapters.isNotEmpty) {
        var currentPlayerPosition = _controller!.value.position.inSeconds;
        // If the chapter was not previously set
        if (currentChapter == null ||
            currentPlayerPosition < currentChapter!.startTime ||
            currentChapter!.endTime < currentPlayerPosition) {
          setState(() {
            currentChapter = metadata.chapters.firstWhereOrNull((chapter) =>
                chapter.startTime <= currentPlayerPosition &&
                currentPlayerPosition < chapter.endTime);
            _refreshMediaMetadata(metadata);
          });
        }
      }
      if (!_controller!.value.isBuffering &&
          flowStep == PlayerFlowStep.buffering) {
        setState(() {
          flowStep = PlayerFlowStep.playerStarted;
        });
      } else if (_controller!.value.isBuffering) {
        setState(() {
          flowStep = PlayerFlowStep.buffering;
        });
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

  void _reinitPlayer(StreamMode streamMode, PlayerMetadata metadata) async {
    final previousPos =
        _controller?.value.position.inSeconds ?? widget.startPosition;
    if (_controller != null) {
      await _controller!.pause();
      await _controller!.dispose();
    }
    final baseUrl = ref.read(apiClientProvider).buildTranscoderUrl(
        "/${base64Encode(utf8.encode(metadata.videoFile.path))}");
    setState(() {
      _controller = VideoPlayerController.networkUrl(
        Uri.parse(streamMode == StreamMode.direct
            ? '$baseUrl/direct'
            : '$baseUrl/master.m3u8'),
        httpHeaders: {"X-CLIENT-ID": widget.clientId},
      )..initialize()
            .then((_) => _onPlayerInit(metadata, previousPos))
            .onError((error, stackTrace) {
          if (streamMode == StreamMode.hls) {
            // ignore: avoid_print
            print("COULD NOT PLAY VIDEO");
          } else {
            _reinitPlayer(StreamMode.hls, metadata);
          }
        });
    });
  }

  void _refreshMediaMetadata(PlayerMetadata m) {
    APIClient client = ref.read(apiClientProvider);
    Mediametadata().setMediaMetadata(
        artistName: m.videoArtist,
        albumName: currentChapter != null ? m.videoTitle : '',
        songName: currentChapter?.name ?? m.videoTitle,
        imageUri: client.buildImageUrl((m.poster ?? m.thumbnail!).id));
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
              _refreshMediaMetadata(m);
              setState(() {
                flowStep = PlayerFlowStep.loadingPlayer;
              });
              _reinitPlayer(StreamMode.direct, m);
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
              duration: const Duration(milliseconds: 200),
              opacity: flowStep == PlayerFlowStep.playerStarted ? 0 : 1,
              child: Thumbnail(
                  image: metadata.value?.thumbnail,
                  disableSlashFadein: true,
                  disableBorderRadius: true),
            ),
            AnimatedOpacity(
              duration: const Duration(milliseconds: 500),
              opacity: flowStep == PlayerFlowStep.playerStarted ||
                      flowStep == PlayerFlowStep.buffering
                  ? 1
                  : 0,
              child: _controller?.value.isInitialized ?? false
                  ? Center(
                      child: AspectRatio(
                      aspectRatio: _controller!.value.aspectRatio,
                      child: VideoPlayer(_controller!),
                    ))
                  : Container(),
            ),
            AnimatedOpacity(
                duration: const Duration(milliseconds: 200),
                opacity: flowStep != PlayerFlowStep.playerStarted ? 1 : 0,
                child: Stack(
                  children: [
                    Container(color: Colors.black.withAlpha(200)),
                    const Center(
                      child: CircularProgressIndicator(),
                    ),
                  ],
                )),
            HideOnInactivity(
                child: PlayerControls(
              chapters: metadata.value?.chapters,
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
