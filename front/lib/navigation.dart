import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
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

  @override
  void initState() {
    GoRouter router = GoRouter.of(context);
    final currentRoute = router.routeInformationProvider.value.uri.toString();
    super.initState();
    for (var tab in tabs) {
      if (tab.initialLocation == currentRoute) {
        _currentIndex = tabs.indexOf(tab);
      }
    }
  }

  static const List<MyNavigationDestination> tabs = [
    MyNavigationDestination(
      icon: FaIcon(FontAwesomeIcons.film),
      label: 'Movies',
      initialLocation: '/packages',
    ),
    MyNavigationDestination(
      icon: FaIcon(FontAwesomeIcons.tv),
      label: 'Videos',
      initialLocation: '/extras',
    ),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(child: widget.child),
      bottomNavigationBar: NavigationBar(
        destinations: tabs,
        onDestinationSelected: (int index) {
          _goOtherTab(context, index);
        },
        selectedIndex: _currentIndex,
      ),
    );
  }

  void _goOtherTab(BuildContext context, int index) {
    GoRouter router = GoRouter.of(context);
    String location = tabs[index].initialLocation;

    setState(() {
      _currentIndex = index;
    });
    router.replace(location);
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
