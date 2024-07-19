<script setup lang="ts">
const route = useRoute();
</script>
<template>
    <div>
        Package {{ route.params.id }}
    </div>
</template>
