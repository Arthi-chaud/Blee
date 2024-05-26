import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:flutter_native_splash/flutter_native_splash.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:blee/router.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';
import 'theme.dart';

void main() {
  // usePathUrlStrategy();
  WidgetsBinding widgetsBinding = WidgetsFlutterBinding.ensureInitialized();
  FlutterNativeSplash.preserve(widgetsBinding: widgetsBinding);
  GoRouter.optionURLReflectsImperativeAPIs = true;
  runApp(const ProviderScope(
    child: MyApp(),
  ));
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    final brightness = MediaQuery.of(context).platformBrightness;
    FlutterNativeSplash.remove();
    return MaterialApp.router(
      title: 'Blee',
      debugShowCheckedModeBanner: false,
      theme: ThemeData.from(
          useMaterial3: true,
          colorScheme: ColorScheme.fromSeed(
              seedColor: AppTheme.seed, brightness: brightness)),
      builder: (context, child) => ResponsiveBreakpoints.builder(
        child: child!,
        breakpoints: Breakpoints.getList(),
      ),
      routerConfig: router,
    );
  }
}
