import 'package:blee/ui/src/breakpoints.dart';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:responsive_framework/responsive_framework.dart';

class ScaffoldWithNavBar extends StatefulWidget {
  final String location;
  const ScaffoldWithNavBar(
      {super.key, required this.child, required this.location});

  final Widget child;

  @override
  State<ScaffoldWithNavBar> createState() => _ScaffoldWithNavBarState();
}

class _ScaffoldWithNavBarState extends State<ScaffoldWithNavBar> {
  int _currentIndex = 1;

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
      icon: FaIcon(FontAwesomeIcons.user),
      label: 'Artists',
      initialLocation: '/artists',
    ),
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
      body: SafeArea(
          child: Row(
        children: [
          ResponsiveBreakpoints.of(context)
                  .largerOrEqualTo(BreakpointEnum.sm.name)
              ? NavigationRail(
                  labelType: NavigationRailLabelType.all,
                  onDestinationSelected: (int index) {
                    _goOtherTab(context, index);
                  },
                  destinations: tabs
                      .map((tab) => tab.toRailDestination(context))
                      .toList(),
                  selectedIndex: _currentIndex)
              : Container(),
          Expanded(child: widget.child)
        ],
      )),
      appBar: AppBar(
        centerTitle: false,
        leadingWidth: ResponsiveBreakpoints.of(context)
                .largerOrEqualTo(BreakpointEnum.sm.name)
            ? 56 + 16
            : null,
        leading: Padding(
          padding: const EdgeInsets.only(left: 8, top: 8, bottom: 8),
          child: Image.asset('web/icons/Icon-192.png'),
        ),
        title: const Text(
          'Blee',
          style: TextStyle(
              fontWeight: FontWeight.w900, fontStyle: FontStyle.italic),
        ),
      ),
      bottomNavigationBar: ResponsiveBreakpoints.of(context)
              .smallerOrEqualTo(BreakpointEnum.xs.name)
          ? NavigationBar(
              destinations: tabs,
              onDestinationSelected: (int index) {
                _goOtherTab(context, index);
              },
              selectedIndex: _currentIndex,
            )
          : null,
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
      super.selectedIcon,
      required super.label})
      : super();
  NavigationRailDestination toRailDestination(BuildContext context) {
    return NavigationRailDestination(
        icon: icon, label: Text(label), selectedIcon: selectedIcon);
  }
}
