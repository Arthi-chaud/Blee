import 'package:blee/navigation.dart';
import 'package:blee/pages/pages.dart';
import 'package:blee/pages/src/artist.dart';
import 'package:blee/pages/src/artists.dart';
import 'package:blee/pages/src/extras.dart';
import 'package:blee/pages/src/player.dart';
import 'package:blee/pages/src/settings.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

final router = GoRouter(
  initialLocation: '/',
  routes: [
    GoRoute(
      path: '/',
      redirect: (_, __) => '/packages',
    ),
    ShellRoute(
        pageBuilder: (
          BuildContext context,
          GoRouterState state,
          Widget child,
        ) {
          return NoTransitionPage(
              child: ScaffoldWithNavBar(
            location: state.matchedLocation,
            child: Padding(
              padding: const EdgeInsets.symmetric(horizontal: 16),
              child: child,
            ),
          ));
        },
        routes: [
          GoRoute(
            path: '/artists',
            pageBuilder: (context, state) =>
                const NoTransitionPage(child: ArtistsPage()),
          ),
          GoRoute(
            path: '/artists/:id',
            pageBuilder: (context, state) => NoTransitionPage(
                child: ArtistPage(artistUuid: state.pathParameters['id']!)),
          ),
          GoRoute(
            path: '/packages',
            pageBuilder: (context, state) =>
                const NoTransitionPage(child: PackagesPage()),
          ),
          GoRoute(
            path: '/packages/:id',
            pageBuilder: (context, state) => NoTransitionPage(
                child: PackagePage(packageUuid: state.pathParameters['id']!)),
          ),
          GoRoute(
            path: '/extras',
            pageBuilder: (context, state) =>
                const NoTransitionPage(child: ExtrasPage()),
          ),
          GoRoute(
            path: '/settings',
            pageBuilder: (context, state) =>
                const NoTransitionPage(child: SettingsPage()),
          ),
        ]),
    GoRoute(
      path: '/player/movie::id',
      pageBuilder: (context, state) {
        var startPositionQuery = state.uri.queryParameters['start_pos'];
        // ignore: avoid_init_to_null
        var startPos = null;

        if (startPositionQuery != null) {
          startPos = int.tryParse(startPositionQuery);
        }
        return NoTransitionPage(
            child: PlayerPage(
          movieUuid: state.pathParameters['id']!,
          startPosition: startPos,
        ));
      },
    ),
    GoRoute(
      path: '/player/extra::id',
      pageBuilder: (context, state) {
        return NoTransitionPage(
            child: PlayerPage(
          extraUuid: state.pathParameters['id']!,
        ));
      },
    ),
  ],
);
