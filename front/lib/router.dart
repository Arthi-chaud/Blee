import 'package:blee/pages/pages.dart';
import 'package:blee/pages/src/extras.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

final router = GoRouter(
  initialLocation: '/packages',
  routes: [
    ShellRoute(
        builder: (
          BuildContext context,
          GoRouterState state,
          Widget child,
        ) {
          return Scaffold(
            appBar: AppBar(
              title: const Text('Blee'),
              backgroundColor: Colors.primaries.first,
            ),
            body: Padding(
              padding: const EdgeInsets.only(left: 16, right: 16),
              child: child,
            ),
          );
        },
        routes: [
          GoRoute(
              path: '/packages',
              builder: (context, state) => const PackagesPage(),
              routes: [
                GoRoute(
                  path: ':id',
                  builder: (context, state) =>
                      PackagePage(packageUuid: state.pathParameters['id']!),
                )
              ]),
          GoRoute(
            path: '/extras',
            builder: (context, state) => const ExtrasPage(),
          ),
        ]),
  ],
);
