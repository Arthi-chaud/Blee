<script setup lang="ts">
const route = useRoute();
</script>
<template>
    <div>Extra {{ route.params.id }}</div>
</template>
