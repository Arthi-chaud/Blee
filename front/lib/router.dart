import 'package:blee/navigation.dart';
import 'package:blee/pages/pages.dart';
import 'package:blee/pages/src/extras.dart';
import 'package:blee/pages/src/player.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

final router = GoRouter(
  initialLocation: '/packages',
  routes: [
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
              path: '/packages',
              pageBuilder: (context, state) =>
                  const NoTransitionPage(child: PackagesPage()),
              routes: [
                GoRoute(
                  path: ':id',
                  pageBuilder: (context, state) => NoTransitionPage(
                      child: PackagePage(
                          packageUuid: state.pathParameters['id']!)),
                )
              ]),
          GoRoute(
            path: '/extras',
            pageBuilder: (context, state) =>
                const NoTransitionPage(child: ExtrasPage()),
          ),
          GoRoute(
            path: '/player/movie::id',
            pageBuilder: (context, state) => NoTransitionPage(
                child: PlayerPage(
              movieUuid: state.pathParameters['id']!,
            )),
          ),
        ]),
    GoRoute(
      path: '/player/extra::id',
      pageBuilder: (BuildContext context, GoRouterState state) {
        return NoTransitionPage(
            child: Scaffold(
                body: PlayerPage(
          extraUuid: state.pathParameters['id']!,
        )));
      },
    ),
  ],
);
