<script lang="ts" setup>
import { VueQueryPlugin } from "@tanstack/vue-query";

app.use(VueQueryPlugin);
</script>

<template>
    <div>
        <NuxtRouteAnnouncer />
        <NuxtWelcome />
    </div>
</template>
