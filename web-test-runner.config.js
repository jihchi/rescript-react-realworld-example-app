// NODE_ENV=test - Needed by "@snowpack/web-test-runner-plugin"
process.env.NODE_ENV = 'test';

module.exports = {
  testRunnerHtml: (testFramework) =>
    `<html>
      <body>
        <script>window.app = { backend: 'http://web-test-runner' }</script>
        <script type="module" src="${testFramework}"></script>
      </body>
    </html>`,
  plugins: [require('@snowpack/web-test-runner-plugin')()],
};
