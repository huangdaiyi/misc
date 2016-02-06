var chronos = angular.module('chronos', []);

chronos.controller('SimulatorController', function($scope, $http){
    $scope.users = [];
    $scope.databases = [{number: 1, instance:'metadata', region:'ap-southeast-1'},
            {number: 2, instance:'metadata', region:'us-east-1'},
            {number: 3, instance:'metadata', region:'us-west-2'},
            {number: 4, instance:'metadata', region:'ap-northeast-1'}];
    $scope.adduser = function(){
        var id = 'Ben_' + Math.floor((Math.random() * 10000) + 1);
        var database = Math.floor((Math.random() * 10000) + 1) % 4;
        var entity = $scope.databases[database];
        $http.post("rest/api/v1/user/"+id+"/" + (database + 1) + "/" + entity.region).success(function(data, status, headers, config){

            if(status == 200){
                $scope.users.splice(0,0, {id: id, database:entity.number, region: entity.region});
            }else{
                console.log("error");
            }
        });

    };
    $scope.loadusers = function(){
        $http.get("rest/api/v1/user").success(function(data, status, headers, config){

            if(status == 200){
                $scope.users = data;
            }else{
                console.log("error");
            }
        });
    };
    $scope.clearusers = function() {
            $http.delete("rest/api/v1/user").success(function(data, status, headers, config){

                if(status != 200){
                    console.log("error");
                }else{
                    $scope.users = [];
                }
        });
    }
});
