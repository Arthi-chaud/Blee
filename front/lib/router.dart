import 'package:blee/pages/pages.dart';
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
            // appBar: AppBar(
            //   title: const Text('Blee'),
            //   backgroundColor: Colors.primaries.first,
            // ),
            body: child,
          );
        },
        routes: [
          GoRoute(
            path: '/packages',
            builder: (context, state) => const PackagePage(),
          )
        ]),
  ],
);
