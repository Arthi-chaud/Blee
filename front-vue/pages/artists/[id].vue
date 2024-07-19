<script setup lang="ts">
const route = useRoute();
</script>
<template>
    <div>
        Artist {{ route.params.id }}
    </div>
</template>
