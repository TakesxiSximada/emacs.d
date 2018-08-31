export default function ({ env, store, route, redirect }) {
  if (process.browser) {
    const token = env.tracking.token;
    // tracking...
  }
}
