<template>
  <span />
</template>


// <script>
//   import { mapState } from 'vuex'

//   export default {
//     computed: {
//       ...mapState({
//         id: state => state.me.id,
//         name: state => state.me.name,
//         email: state => state.me.email,
//       })
//     },
//     mounted () {
//       console.log('FIRE')
//       if (process.browser) {
//         window.intercomSettings = {
//           app_id: "zurh06e0",
//           user_id: `${this.id}`,
//           name: `${this.name}`,
//           email: `${this.email}`,
//         };

//         (function(){
//           console.log('fire');
//           var w=window;
//           var ic=w.Intercom;
//           if(typeof ic==="function"){
//             ic('reattach_activator');
//             ic('update',intercomSettings);
//           }else{
//             var d=document;
//             var i=function(){
//               i.c(arguments)
//             };
//             i.q=[];
//             i.c=function(args){
//               i.q.push(args)
//             };
//             w.Intercom=i;
//             function l(){
//               var s=d.createElement('script');
//               s.type='text/javascript';
//               s.async=true;
//               s.src='https://widget.intercom.io/widget/zurh06e0';
//               var x=d.getElementsByTagName('script')[0];
//               x.parentNode.insertBefore(s,x);
//             }
//             if(w.attachEvent){
//               w.attachEvent('onload',l);
//             }else{
//               w.addEventListener('load',l,false);
//             }
//           }
//         })();
//       }
//     },
//   }
// </script>
