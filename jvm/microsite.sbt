/* Microsite Settings
 *
 * To generate the microsite locally, use `sbt makeMicrosite`.
 * To publish the site to Github, use `sbt publishMicrosite`.
 *
 * Spark deps must not be marked `provided` while doing these, or you will get errors.
 */
enablePlugins(MicrositesPlugin)
enablePlugins(SiteScaladocPlugin)

micrositeName := "MAML"
micrositeDescription := "Map Algebra Model Language"
micrositeAuthor := "GeoTrellis Team at Azavea"
micrositeGitterChannel := false
micrositeOrganizationHomepage := "https://www.azavea.com/"
micrositeGithubOwner := "geotrellis"
micrositeGithubRepo := "maml"
micrositeBaseUrl := "/maml"
micrositeDocumentationUrl := "/maml/latest/api" /* Location of Scaladocs */
