const { DownloaderHelper } = require('node-downloader-helper');
const {map} = require('already')
const path = require('path')
const {writeFile} = require('fs/promises')
const forge = require('mappersmith').default

const frdb = forge({
  host: "https://api.fiveringsdb.com",
  resources:{
    Cards: {
      fetchAll: {method:"GET",path: "/cards"}
    }
  }
})


async function download(url, dir, fileName) {
  return new Promise((resolve,reject) => {
    const dl = new DownloaderHelper(url, dir, {fileName});
    dl.on('end',resolve)
    dl.on('error',reject)
    dl.start();
  })
}



async function main () {
  const res = await frdb.Cards.fetchAll()
  const data = res.data()
  const cards = data.records.map(c => ({
    glory:c.glory,
    military:c.military,
    political:c.political,
    military_bonus:c.military_bonus,
    political_bonus:c.political_bonus,
    cost:c.cost,
    influence_cost:c.influence_cost,
    role_restrictions:c.role_restrictions,
    element:c.element,
    influence_pool:c.influence_pool,
    fate:c.fate,
    honor:c.honor,
    strength_bonus:c.strength_bonus,
    strength:c.strength,
    traits:c.traits,
    name:c.name,
    id:c.id,
    text_canonical:c.text_canonical,
    unicity:c.unicity,
    clan:c.clan,
    cycle:c.pack_cards[0].pack.id,
    card_number:c.pack_cards[0].position,
    artist:c.pack_cards[0].illustrator,
    image:c.pack_cards[0].image_url,
  }))

  await writeFile("./public/assets/cards.json", JSON.stringify(cards))

  await map(cards, {concurrency:5}, async (card) => {
    const ext = path.extname(card.image)
    const folder = path.join( __dirname,"..",'public','assets' )
    const filename = `card-${card.id}${ext}`
    await download(card.image, folder,filename)
    return filename
  })





}

main().then(()=>process.exit())