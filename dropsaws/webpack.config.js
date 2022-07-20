const path = require('path');


module.exports = {
    entry: 'src/index.js',
    output: {
        filename: 'dropsaws.js',
        path: path.join(__dirname, 'dist'),
        libraryTarget: 'umd',
        library: 'dropsaws',
    },
    module: {
        loaders: [
            {test: /\.css$/, loader: 'style!css'},
            {
                loader: 'msx-loader',
                test: /\.msx/,
                exclude: /(node_modules|bower_components)/,
            },
            {
                loader: 'babel',
                test: /\.js$/,
                exclude: /node_modules/,
                query: {
                    presets: ['es2015']
                }
            },
        ]
    },
    resolve: {
        extensions: [
            '', '.js', '.msx',
        ],
        root: [
            __dirname,
            path.join(__dirname, 'node_modules'),
        ],
    }
}
