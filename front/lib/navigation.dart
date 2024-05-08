import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

class ScaffoldWithNavBar extends StatefulWidget {
  final String location;
  const ScaffoldWithNavBar(
      {super.key, required this.child, required this.location});

  final Widget child;

  @override
  State<ScaffoldWithNavBar> createState() => _ScaffoldWithNavBarState();
}

class _ScaffoldWithNavBarState extends State<ScaffoldWithNavBar> {
  int _currentIndex = 0;

  static const List<MyNavigationDestination> tabs = [
    MyNavigationDestination(
      icon: Icon(Icons.movie),
      label: 'Movies',
      initialLocation: '/packages',
    ),
    MyNavigationDestination(
      icon: Icon(Icons.music_video_sharp),
      label: 'Videos',
      initialLocation: '/extras',
    ),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(child: widget.child),
      bottomNavigationBar: NavigationBar(
        height: 70,
        destinations: tabs,
        onDestinationSelected: (int index) {
          _goOtherTab(context, index);
        },
        selectedIndex: _currentIndex,
      ),
    );
  }

  void _goOtherTab(BuildContext context, int index) {
    if (index == _currentIndex) return;
    GoRouter router = GoRouter.of(context);
    String location = tabs[index].initialLocation;

    setState(() {
      _currentIndex = index;
    });
    router.go(location);
  }
}

class MyNavigationDestination extends NavigationDestination {
  final String initialLocation;

  const MyNavigationDestination(
      {super.key,
      required this.initialLocation,
      required super.icon,
      required super.label})
      : super(selectedIcon: icon);
}