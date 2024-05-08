import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:blee/router.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

void main() {
  // usePathUrlStrategy();
  GoRouter.optionURLReflectsImperativeAPIs = true;
  runApp(const ProviderScope(
    child: MyApp(),
  ));
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp.router(
      title: 'Blee',
      theme: ThemeData(
          colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
          useMaterial3: true,
          fontFamily: 'Rubik'),
      builder: (context, child) => ResponsiveBreakpoints.builder(
        child: child!,
        breakpoints: Breakpoints.getList(),
      ),
      routerConfig: router,
    );
  }
}
